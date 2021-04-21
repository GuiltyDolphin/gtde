;;; org-gtd-transient.el --- org-gtd transient interface.

;; Copyright (C) 2020-2021 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Defines org-gtd functionality for interfacing with transient.
;;;
;;; Code:


(eval-when-compile (require 'subr-x))

(require 'dash)
(require 'transient)


;;; Classes


(defclass org-gtd-transient--reader ()
  ((multi-value :initarg :multi-value :initform nil
                :documentation "T if multiple values can be read at once.")
   (prompt :initarg :prompt :documentation "Prompt to use when reading values.")
   (choices :initarg :choices :initform nil
            :documentation "Alternatives that can be selected from."))
  :documentation "Base class for readers.")

(defclass org-gtd-transient--component (transient-child)
  (
   ;; NOTE: I've had to add these 'inapt' fields as they are part of
   ;; transient-suffix, but not transient-child, and errors were being
   ;; thrown without them. The transient docs c (2020-08-26)
   (inapt                             :initform nil)
   (inapt-if
    :initarg :inapt-if
    :initform nil
    :documentation "Inapt if predicate returns non-nil.")
   (inapt-if-not
    :initarg :inapt-if-not
    :initform nil
    :documentation "Inapt if predicate returns nil.")
   (inapt-if-non-nil
    :initarg :inapt-if-non-nil
    :initform nil
    :documentation "Inapt if variable's value is non-nil.")
   (inapt-if-nil
    :initarg :inapt-if-nil
    :initform nil
    :documentation "Inapt if variable's value is nil.")
   (inapt-if-mode
    :initarg :inapt-if-mode
    :initform nil
    :documentation "Inapt if major-mode matches value.")
   (inapt-if-not-mode
    :initarg :inapt-if-not-mode
    :initform nil
    :documentation "Inapt if major-mode does not match value.")
   (inapt-if-derived
    :initarg :inapt-if-derived
    :initform nil
    :documentation "Inapt if major-mode derives from value.")
   (inapt-if-not-derived
    :initarg :inapt-if-not-derived
    :initform nil
    :documentation "Inapt if major-mode does not derive from value."))
  :documentation "Abstract class for individual components."
  :abstract t)

(defclass org-gtd-transient--non-interactive (org-gtd-transient--component)
  (())
  :documentation "Abstract class for non-interactive components.

Objects that inherit from this class are not expected to provide a key."
  :abstract t)

(defclass org-gtd-transient--targeted (org-gtd-transient--component)
  ((target-id :initarg :target-id :documentation "Identifier of the target component."))
  :documentation "Class for components which can target others."
  :abstract t)

(cl-defgeneric org-gtd-transient--target (obj)
  "Determine the target component of OBJ.")

(cl-defgeneric org-gtd-transient--set-target-value (obj val &optional tactic)
  "Set the value of OBJ's target to VAL.

TACTIC, if specified, determines how to combine existing and new values.")

(defclass org-gtd-transient--display (org-gtd-transient--targeted org-gtd-transient--non-interactive)
  ((description :initarg :description :documentation "Description of the component.")
   (command :initarg :command :initform ignore)
   (format :initarg :format :initform "%d %v"))
  :documentation "Class for displaying the value of another component.")

(defclass org-gtd-transient--setter (transient-suffix org-gtd-transient--targeted)
  ((transient :initarg :transient :initform 'transient--do-call)
   (reader :initarg :reader :documentation "Configuration for reading values."))
  :documentation "Interactive element that reads a value from the user, and sets an in-scope variable based on that value.")


;;;; Dates


(defclass org-gtd-transient--date-reader (org-gtd-transient--reader)
  (())
  :documentation "Class for reading dates.")

(defclass org-gtd-transient--date-display (org-gtd-transient--display)
  ((date-format :initarg :date-format :initform "%c" :documentation "Format used to display the date (see `format-time-string' for available formats).")
   (date-format-no-time :initarg :date-format-no-time :documentation "Format used to display the date when there is no time specified. Defaults to use the :date-format property."))
  :documentation "Class for displaying dates.")

(defclass org-gtd--date ()
  ((time-string :initarg :time-string)
   (time-specified :initarg :time-specified :documentation "Whether the user specified a time and not just a date.")
   (internal-time :initarg :internal-time :documentation "Internal time value represented by the date."))
  :documentation "Represents a date entered by a user.")


;;; Init


(cl-defmethod transient-init-scope ((_ org-gtd-transient--component))
  "Noop." nil)

(defun org-gtd-transient--var-env ()
  "Get the variable environment for the current prefix."
  (oref transient--prefix scope))

(defun org-gtd-transient--set-var-env (env)
  "Set the current variable environment to ENV."
  (oset transient--prefix scope env))

(defun org-gtd-transient--var-val (var)
  "Get the value of VAR in the current environment."
  (lax-plist-get (org-gtd-transient--var-env) var))

(defun org-gtd-transient--set-var-val (var val)
  "Set the value of VAR to VAL in the current environment."
  (org-gtd-transient--set-var-env
   (lax-plist-put (org-gtd-transient--var-env) var val)))

(defun org-gtd-transient--initialize-variable (var)
  "Initialize VAR for the current prefix environment."
  (let ((var-vals (org-gtd-transient--var-env)))
    (unless (plist-member var-vals var)
      (org-gtd-transient--set-var-val var nil))))

(cl-defmethod transient-init-scope ((obj org-gtd-transient--targeted))
  (org-gtd-transient--initialize-variable (oref obj target-id)))

(cl-defmethod transient--init-suffix-key ((_ org-gtd-transient--non-interactive)))


;;; Targeting


(cl-defmethod org-gtd-transient--target ((obj org-gtd-transient--targeted))
  "Get the target of the current object."
  (oref obj target-id))

(defun org-gtd-transient--target-value (obj)
  "Get the value of the target of OBJ."
  (org-gtd-transient--var-val (org-gtd-transient--target obj)))


;;; Read


(cl-defgeneric org-gtd-transient--read (obj)
  "Read a value according to the specification of OBJ.")

(cl-defmethod org-gtd-transient--read ((reader org-gtd-transient--reader) &optional value)
  "Read a value according to the specification of READER.

VALUE, if specified, indicates the existing value of the target being read for."
  (with-slots (choices multi-value prompt) reader
    (let* ((overriding-terminal-local-map nil)
           (value-str
            (if multi-value
                (mapconcat (lambda (v) (format "%s" v)) value ",") (format "%s" value)))
           (history-key nil)
           (transient--history (alist-get history-key transient-history))
           (transient--history (if (or (null value-str)
                                       (equal value-str (car transient--history)))
                                   transient--history
                                 (cons value-str transient--history)))
           (initial-input (and transient-read-with-initial-input
                               (car transient--history)))
           (history (if initial-input
                        (cons 'transient--history 1)
                      'transient--history))
           (value
            (cond
             (multi-value
              (completing-read-multiple prompt choices nil nil
                                        initial-input history))
             (choices
              (completing-read prompt choices nil t initial-input history))
             (t (read-string prompt initial-input history)))))
      (when value
        (when (bound-and-true-p ivy-mode)
          (set-text-properties 0 (length (car transient--history)) nil
                               (car transient--history)))
        (setf (alist-get history-key transient-history)
              (delete-dups transient--history)))
      value)))

(cl-defmethod org-gtd-transient--read ((reader org-gtd-transient--date-reader) &optional value)
  "Read a date value according to the specification of READER.

VALUE, if specified, indicates the existing value of the target being read for."
  (with-slots (prompt) reader
    (let* ((user-date (org-read-date nil nil nil prompt))
           (time-specified (string-match-p ":" user-date)))
      (org-gtd--date :time-string user-date :time-specified time-specified :internal-time (org-time-string-to-time user-date)))))

(cl-defmethod org-gtd-transient--read ((obj org-gtd-transient--setter))
  (org-gtd-transient--read (oref obj reader) (org-gtd-transient--target-value obj)))


;;; Set


(cl-defmethod org-gtd-transient--set-target-value ((obj org-gtd-transient--setter) val &optional tactic)
  "Set the value of the current setter target to VAL.

If specified, use TACTIC instead of the merge tactic of the setter's target."
  (org-gtd-transient--set-var-val (org-gtd-transient--target obj) val))


;;; Draw


(cl-defgeneric org-gtd-transient--format-value-pretty (obj)
  "Format the value of OBJ in as pretty a manner as possible.")

(cl-defmethod transient-format ((obj org-gtd-transient--display))
  "Return a string generated using OBJ's `format'.
%d is formatted using `transient-format-description'.
%v is formatted using `transient-format-value'.
%V is formatted using `org-gtd-transient--format-value-pretty'."
  (format-spec (oref obj format)
               `((?d . ,(transient-format-description obj))
                 (?v . ,(transient-format-value obj))
                 (?V . ,(org-gtd-transient--format-value-pretty obj)))))

(cl-defmethod transient-format-description ((obj org-gtd-transient--display))
  "Format the description by calling the next method.

If the result doesn't use the `face' property at all, then apply
the face `transient-heading' to the complete string."
  (when-let ((desc (cl-call-next-method obj)))
    (if (text-property-not-all 0 (length desc) 'face nil desc)
        desc
      (propertize desc 'face 'transient-heading))))

(defun org-gtd--propertize-with-defaults (s v)
  "Propertize string S formatted from value V, treating it as inactive if V is NIL."
  (propertize s 'face (if v 'transient-value 'transient-inactive-value)))

(cl-defmethod transient-format-value ((obj org-gtd-transient--display))
  "When formatting a value for a display component, we display the value of the target."
  (let ((val (org-gtd-transient--target-value obj)))
    (org-gtd--propertize-with-defaults (format "%s" val) val)))

(cl-defmethod org-gtd-transient--format-value-pretty ((obj org-gtd-transient--display))
  "When formatting a value for a display component, we display the value of the target."
  (let ((value (org-gtd-transient--target-value obj))
        (propertize-value (lambda (v) (propertize (format "%s" v) 'face 'transient-value))))
    (if value
        (cond
         ;; got a date, try and format it nicely
         ((org-gtd--date-child-p value)
          (funcall propertize-value (format-time-string (if (oref value time-specified) "%FT%H:%M" "%F") (oref value internal-time))))
         ;; with a list, display on multiple lines if there are multiple elements, otherwise display on a single line
         ((listp value)
          (if (cdr value)
              ;; display elements on separate lines
              (mapconcat (lambda (v) (concat "\n     " (funcall propertize-value v))) value "")
            ;; if only one element, display it inline
            (funcall propertize-value (car value))))
         ;; not a list, just display the value
         (t (funcall propertize-value value)))
      (propertize "unset" 'face 'transient-inactive-value))))

(cl-defmethod org-gtd-transient--format-value-pretty ((obj org-gtd-transient--date-display))
  "Pretty display of a date."
  (with-slots (date-format date-format-no-time) obj
    (let ((val (org-gtd-transient--target-value obj)))
      (org-gtd--propertize-with-defaults
       (if val (format-time-string (if (oref val time-specified) date-format
                                     (or (and (slot-boundp obj 'date-format-no-time) date-format-no-time) date-format))
                                   (oref val internal-time)) "") val))))


(provide 'org-gtd-transient)
;;; org-gtd-transient.el ends here
