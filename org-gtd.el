;;; org-gtd.el --- Tools for working with GTD and org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>
;; URL: https://github.com/GuiltyDolphin/org-gtd
;; Git-Repository: git://github.com/GuiltyDolphin/org-gtd.git
;; Created: 2020-05-03
;; Version: 0.0.0
;; Keywords: outlines, tools
;; Package-Requires: ((dash "2.17.0") (emacs "25.1") (transient "0.2.0"))

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

;; org-gtd provides tools for working with GTD workflows and org-mode.
;;
;; For more information see the README.

;;; Code:


(eval-when-compile (require 'subr-x))

(require 'dash)
(require 'transient)


;;; Classes


(defclass org-gtd-transient--component (transient-child)
  (())
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

(defclass org-gtd-transient--value (org-gtd-transient--non-interactive)
  ((default :initarg :default)
   (id :initarg :id :documentation "ID of the component. Must be unique for the current prefix.")
   (command :initarg :command :initform ignore)
   (value :initform nil)
   (combine-tactic :initarg :combine-tactic :initform replace
                   :documentation "Tactic for combining old and new values.

Can be one of `replace' or `merge'. `replace' means to replace the existing value
with the new value. `merge' means to try and combine the old and new values; for lists,
`merge' will cause the new value to be appended to the old, unless `multi-value' is non-NIL,
in which case the new and old values are merged as lists.")
   (multi-value :initarg :multi-value :initform nil :type booleanp
                :documentation "If set to T, will treat a list of values as being a list of values rather than a single value when setting. Set this to T if the user can enter multiple values at a time."))
  :documentation "Class for components that hold a value.")

(defclass org-gtd-transient--display (org-gtd-transient--targeted org-gtd-transient--non-interactive)
  ((description :initarg :description :documentation "Description of the component.")
   (command :initarg :command :initform ignore)
   (format :initarg :format :initform "%d %v"))
  :documentation "Class for displaying the value of another component.")

(defclass org-gtd-transient--setter (transient-suffix org-gtd-transient--targeted)
  ((transient :initarg :transient :initform 'transient--do-call))
  :documentation "Class for suffixes that set the value of other infixes.")


;;; Init


(cl-defmethod transient-init-scope ((_ org-gtd-transient--component))
  "Noop." nil)

(cl-defmethod transient--init-suffix-key ((_ org-gtd-transient--non-interactive)))

(cl-defmethod transient-init-value ((obj org-gtd-transient--value))
  (when (slot-boundp obj 'default)
    (oset obj value (oref obj default))))


;;; Targeting


(defun org-gtd-transient--current-components ()
  "Return a list of all the components in the current prefix."
  (cl-labels ((s (def)
                 (cond
                  ((stringp def) nil)
                  ((listp def) (cl-mapcan #'s def))
                  ((org-gtd-transient--component--eieio-childp def)
                   (list def))
                  ((transient-group--eieio-childp def)
                   (cl-mapcan #'s (oref def suffixes)))
                  ((transient-suffix--eieio-childp def)
                   (list def)))))
    (cl-mapcan #'s transient--layout)))

(defun org-gtd-transient--components-with-ids ()
  "Get an alist of components for the current prefix, paired with their ID.

Only components with an ID specified will appear in the alist."
  (let ((components (org-gtd-transient--current-components)))
    (-non-nil (mapcar (lambda (obj) (when (and (slot-exists-p obj 'id) (slot-boundp obj 'id)) (cons (oref obj id) obj))) components))))

(defun org-gtd-transient--get-object-for-id (id)
  "Get the object associated with the id ID."
  (alist-get id (org-gtd-transient--components-with-ids) nil nil #'equal))

(cl-defmethod org-gtd-transient--target ((obj org-gtd-transient--targeted))
  "Get the target of the current object."
  (let* ((target-id (oref obj target-id)))
    (org-gtd-transient--get-object-for-id target-id)))


;;; Set


(cl-defmethod transient-infix-set ((obj org-gtd-transient--value) value &optional tactic)
  (let ((tactic (or tactic (oref obj combine-tactic)))
        (old-val (oref obj value)))
    (if (eq tactic 'replace)
        ;; replace tactic: entire value gets replaced
        (oset obj value value)
      ;; merge tactic
      (if (oref obj multi-value)
          ;; value is a list (multi-value), so we concatenate
          (oset obj value (-concat old-val value))
        ;; value is not a list, so we append (merge tactic forces the result to be a list)
        (oset obj value (-snoc old-val value))))))

(cl-defmethod org-gtd-transient--set-target-value ((obj org-gtd-transient--setter) val &optional tactic)
  "Set the value of the current setter target to VAL.

If specified, use TACTIC instead of the merge tactic of the setter's target."
  (transient-infix-set (org-gtd-transient--target obj) val tactic))


;;; Draw


(cl-defmethod transient-format ((obj org-gtd-transient--value))
  "Components that exist only to hold values are not rendered by default."
  "")

(cl-defmethod transient-format ((obj org-gtd-transient--display))
  "Return a string generated using OBJ's `format'.
%d is formatted using `transient-format-description'.
%v is formatted using `transient-format-value'."
  (format-spec (oref obj format)
               `((?d . ,(transient-format-description obj))
                 (?v . ,(transient-format-value obj)))))

(cl-defmethod transient-format-description ((obj org-gtd-transient--display))
  "Format the description by calling the next method.  If the result
doesn't use the `face' property at all, then apply the face
`transient-heading' to the complete string."
  (when-let ((desc (cl-call-next-method obj)))
    (if (text-property-not-all 0 (length desc) 'face nil desc)
        desc
      (propertize desc 'face 'transient-heading))))

(cl-defmethod transient-format-value ((obj org-gtd-transient--display))
  "When formatting a value for a display component, we display the value of the target."
  (let ((val (oref (org-gtd-transient--target obj) value)))
    (propertize (format "%s" val) 'face (if val 'transient-value 'transient-inactive-value))))


(provide 'org-gtd)
;;; org-gtd.el ends here
