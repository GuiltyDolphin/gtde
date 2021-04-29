;;; gtde-transient.el --- gtde transient interface.

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
;;; Defines gtde functionality for interfacing with transient.
;;;
;;; Code:


(eval-when-compile (require 'subr-x))

(require 'dash)
(require 'transient)


;;; Helpers


(defun gtde--map-put (map key value)
  "Set the value of KEY in MAP to VALUE."
  (let ((elt (assoc key map)))
    (if elt (progn (setcdr elt value) map) (cons (cons key value) map))))

(defun gtde--map-get (map key)
  "Get the value at KEY in MAP, if any."
  (alist-get key map nil nil #'equal))

(defun gtde--map-member (map key)
  "Return non-NIL if there is an entry for KEY in MAP."
  (assoc key map))


;;; Classes


(defclass gtde-transient--reader ()
  ((multi-value :initarg :multi-value :initform nil
                :documentation "T if multiple values can be read at once.")
   (prompt :initarg :prompt :documentation "Prompt to use when reading values.")
   (choices :initarg :choices :initform nil
            :documentation "Alternatives that can be selected from. Can be a zero-argument function that generates a list, or a list.")
   (require-match :initarg :require-match
                  :initform nil
                  :documentation "Specifies whether the user's entry must match a provided choice. See `completing-read' for the exact behaviour.")
   (on-result :initarg :on-result :initform nil
              :documentation "Function used to transform the value read."))
  :documentation "Base class for readers.")

(defclass gtde-transient--component (transient-child)
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

(defclass gtde-transient--non-interactive (gtde-transient--component)
  (())
  :documentation "Abstract class for non-interactive components.

Objects that inherit from this class are not expected to provide a key."
  :abstract t)

(defclass gtde-transient--targeted (gtde-transient--component)
  ((target-id :initarg :target-id :documentation "Identifier of the target component."))
  :documentation "Class for components which can target others."
  :abstract t)

(cl-defgeneric gtde-transient--target (obj)
  "Determine the target component of OBJ.")

(cl-defgeneric gtde-transient--set-target-value (obj val &optional tactic)
  "Set the value of OBJ's target to VAL.

TACTIC, if specified, determines how to combine existing and new values.")

(defclass gtde-transient--display (gtde-transient--targeted gtde-transient--non-interactive)
  ((description :initarg :description :documentation "Description of the component.")
   (command :initarg :command :initform ignore)
   (format :initarg :format :initform "%d %v")
   (renderer :initarg :renderer :initform nil
             :documentation "Optional function used to render the targeted value."))
  :documentation "Class for displaying the value of another component.")

(defclass gtde-transient--setter (transient-suffix gtde-transient--targeted)
  ((transient :initarg :transient :initform 'transient--do-call)
   (reader :initarg :reader :documentation "Configuration for reading values."))
  :documentation "Interactive element that reads a value from the user, and sets an in-scope variable based on that value.")


;;;; Dates


(defclass gtde-transient--date-reader (gtde-transient--reader)
  (())
  :documentation "Class for reading dates.")

(defclass gtde-transient--date-display (gtde-transient--display)
  ((date-format :initarg :date-format :initform "%c" :documentation "Format used to display the date (see `format-time-string' for available formats).")
   (date-format-no-time :initarg :date-format-no-time :documentation "Format used to display the date when there is no time specified. Defaults to use the :date-format property."))
  :documentation "Class for displaying dates.")

(defclass gtde--date ()
  ((time-string :initarg :time-string)
   (time-specified :initarg :time-specified :documentation "Whether the user specified a time and not just a date.")
   (internal-time :initarg :internal-time :documentation "Internal time value represented by the date."))
  :documentation "Represents a date entered by a user.")


;;; Init


(cl-defmethod transient-init-scope ((_ gtde-transient--component))
  "Noop." nil)

(defun gtde-transient--var-env ()
  "Get the variable environment for the current prefix."
  (oref transient--prefix scope))

(defun gtde-transient--set-var-env (env)
  "Set the current variable environment to ENV."
  (oset transient--prefix scope env))

(defun gtde-transient--var-val (var)
  "Get the value of VAR in the current environment."
  (gtde--map-get (gtde-transient--var-env) var))

(defun gtde-transient--set-var-val (var val)
  "Set the value of VAR to VAL in the current environment."
  (gtde-transient--set-var-env
   (gtde--map-put (gtde-transient--var-env) var val)))

(defun gtde-transient--initialize-variable (var)
  "Initialize VAR for the current prefix environment."
  (let ((var-vals (gtde-transient--var-env)))
    (unless (gtde--map-member var-vals var)
      (gtde-transient--set-var-val var nil))))

(cl-defmethod transient-init-scope ((obj gtde-transient--targeted))
  (gtde-transient--initialize-variable (oref obj target-id)))

(cl-defmethod transient--init-suffix-key ((_ gtde-transient--non-interactive)))


;;; Targeting


(cl-defmethod gtde-transient--target ((obj gtde-transient--targeted))
  "Get the target of the current object."
  (oref obj target-id))

(defun gtde-transient--target-value (obj)
  "Get the value of the target of OBJ."
  (gtde-transient--var-val (gtde-transient--target obj)))


;;; Render


(cl-defgeneric gtde-transient--render-to-input-text (obj)
  "Render OBJ to input text for user entry and display.

The rendered form should be suitable for re-parsing into an equal object (in the current state).")

(cl-defmethod gtde-transient--render-to-input-text ((obj gtde--status))
  (oref obj display))

(cl-defmethod gtde-transient--render-to-input-text ((obj t))
  "Resort to basic formatting if OBJ doesn't match any other case."
  (format "%s" obj))


;;; Read


(defvar gtde-transient--history nil "Completion history for transients.")

(defun gtde-transient--get-history (prefix suffix)
  "Get the history for SUFFIX under PREFIX."
  (copy-alist (alist-get (oref suffix command) (alist-get (oref prefix command) gtde-transient--history))))

(defun gtde-transient--add-to-history (prefix suffix value)
  "Add VALUE to the history for SUFFIX under PREFIX."
  (let ((hist (gtde-transient--get-history prefix suffix)))
    (setf (alist-get (oref suffix command) (alist-get (oref prefix command) gtde-transient--history)) (-uniq (cons value hist)))))

(cl-defgeneric gtde-transient--read (obj)
  "Read a value according to the specification of OBJ.")

(cl-defmethod gtde-transient--read ((reader gtde-transient--reader) &optional value)
  "Read a value according to the specification of READER.

VALUE, if specified, indicates the existing value of the target being read for."
  (with-slots (choices multi-value prompt on-result require-match) reader
    (let* ((prefix transient-current-prefix)
           (suffix (transient-suffix-object))
           (overriding-terminal-local-map nil)
           (choices (if (functionp choices) (funcall choices) choices))
           (value-str
            (if multi-value
                (mapconcat (lambda (v) (gtde-transient--render-to-input-text v)) value ",") (gtde-transient--render-to-input-text value)))
           (history-raw (gtde-transient--get-history prefix suffix))
           (history-with-value (if (or (null value-str)
                                       (equal value-str (car history-raw)))
                                   history-raw
                                 (cons value-str history-raw)))
           (initial-input (and transient-read-with-initial-input
                               (car history-with-value)))
           (history (if initial-input
                        (cons 'history-with-value 1)
                      'history-with-value))
           (value
            (cond
             (multi-value
              (completing-read-multiple prompt choices nil require-match
                                        initial-input history))
             (choices
              (completing-read prompt choices nil require-match initial-input history))
             (t (read-string prompt initial-input history)))))
      (when value
        (when (bound-and-true-p ivy-mode)
          (set-text-properties 0 (length (car transient--history)) nil
                               (car transient--history)))
        (gtde-transient--add-to-history prefix suffix value))
      (if on-result (funcall on-result value) value))))

(cl-defmethod gtde-transient--read ((reader gtde-transient--date-reader) &optional value)
  "Read a date value according to the specification of READER.

VALUE, if specified, indicates the existing value of the target being read for."
  (with-slots (prompt) reader
    (let* ((user-date (org-read-date nil nil nil prompt))
           (time-specified (string-match-p ":" user-date)))
      (gtde--date :time-string user-date :time-specified time-specified :internal-time (org-time-string-to-time user-date)))))

(cl-defmethod gtde-transient--read ((obj gtde-transient--setter))
  (gtde-transient--read (oref obj reader) (gtde-transient--target-value obj)))


;;; Set


(cl-defmethod gtde-transient--set-target-value ((obj gtde-transient--setter) val &optional tactic)
  "Set the value of the current setter target to VAL.

If specified, use TACTIC instead of the merge tactic of the setter's target."
  (gtde-transient--set-var-val (gtde-transient--target obj) val))


;;; Draw


(cl-defgeneric gtde-transient--format-value-pretty (obj)
  "Format the value of OBJ in as pretty a manner as possible.")

(cl-defmethod transient-format ((obj gtde-transient--display))
  "Return a string generated using OBJ's `format'.
%d is formatted using `transient-format-description'.
%v is formatted using `transient-format-value'.
%V is formatted using `gtde-transient--format-value-pretty'."
  (format-spec (oref obj format)
               `((?d . ,(transient-format-description obj))
                 (?v . ,(transient-format-value obj))
                 (?V . ,(gtde-transient--format-value-pretty obj)))))

(cl-defmethod transient-format-description ((obj gtde-transient--display))
  "Format the description by calling the next method.

If the result doesn't use the `face' property at all, then apply
the face `transient-heading' to the complete string."
  (when-let ((desc (cl-call-next-method obj)))
    (if (text-property-not-all 0 (length desc) 'face nil desc)
        desc
      (propertize desc 'face 'transient-heading))))

(defun gtde--propertize-with-defaults (s v)
  "Propertize string S formatted from value V, treating it as inactive if V is NIL."
  (propertize s 'face (if v 'transient-value 'transient-inactive-value)))

(cl-defmethod transient-format-value ((obj gtde-transient--display))
  "When formatting a value for a display component, we display the value of the target."
  (let* ((val (gtde-transient--target-value obj))
         (renderer (oref obj renderer))
         (val-rendered (if (functionp renderer) (funcall renderer val) val)))
    (gtde--propertize-with-defaults (format "%s" val-rendered) val-rendered)))

(cl-defmethod gtde-transient--format-value-pretty ((obj gtde-transient--display))
  "When formatting a value for a display component, we display the value of the target."
  (let* ((value (gtde-transient--target-value obj))
         (renderer (oref obj renderer))
         (propertize-value (lambda (v)
                             (let ((val-rendered (if renderer (funcall renderer v) v)))
                               (propertize (format "%s" val-rendered) 'face 'transient-value)))))
    (if value
        (cond
         ;; got a date, try and format it nicely
         ((cl-typep value #'gtde--date)
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

(cl-defmethod gtde-transient--format-value-pretty ((obj gtde-transient--date-display))
  "Pretty display of a date."
  (with-slots (date-format date-format-no-time) obj
    (let ((val (gtde-transient--target-value obj)))
      (gtde--propertize-with-defaults
       (if val (format-time-string (if (oref val time-specified) date-format
                                     (or (and (slot-boundp obj 'date-format-no-time) date-format-no-time) date-format))
                                   (oref val internal-time)) "") val))))


(provide 'gtde-transient)
;;; gtde-transient.el ends here
