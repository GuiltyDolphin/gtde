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


(defclass org-gtd-transient--display (transient-child)
  ((id :initarg :id :documentation "ID of the component. Must be unique for the current prefix.")
   (description :initarg :description :documentation "Description of the component.")
   (command :initarg :command :initform ignore)
   (format :initarg :format :initform "%d %v")
   (value :initform nil)
   (combine-tactic :initarg :combine-tactic :initform replace
                   :documentation "Tactic for combining old and new values.

Can be one of `replace' or `merge'. `replace' means to replace the existing value
with the new value. `merge' means to try and combine the old and new values; for lists,
`merge' will cause the new value to be appended to the old, unless `multi-value' is non-NIL,
in which case the new and old values are merged as lists.")
   (multi-value :initarg :multi-value :initform nil :type booleanp
                :documentation "If set to T, will treat a list of values as being a list of values rather than a single value when setting. Set this to T if the user can enter multiple values at a time."))
  :documentation "Class for transient components that hold values.")

(defclass org-gtd-transient--setter (transient-suffix)
  ((target-id :initarg :target-id :documentation "Id of the infix this targets.")
   (transient :initarg :transient :initform 'transient--do-call))
  :documentation "Class for suffixes that set the value of other infixes.")

(defclass org-gtd-transient--value (transient-variable)
  ((default :initarg :default)
   (id :initarg :id)
   (toggle-set :initarg :toggle-set :initform nil)
   (combine-tactic :initarg :combine-tactic :initform replace
                   :documentation "Tactic for combining old and new values.

Can be one of `replace' or `merge'. `replace' means to replace the existing value
with the new value. `merge' means to try and combine the old and new values; for lists,
`merge' will cause the new value to be appended to the old, unless `multi-value' is non-NIL,
in which case the new and old values are merged as lists."))
  :description "Value that can be set in a transient.")


;;; Init


(cl-defmethod transient-init-scope ((_   org-gtd-transient--display))
  "Noop." nil)

(cl-defmethod transient--init-suffix-key ((obj org-gtd-transient--display)))

(cl-defmethod transient-init-value ((obj org-gtd-transient--value))
  (when (slot-boundp obj 'default)
    (oset obj value (oref obj default))))


;; Read


(cl-defmethod transient-infix-read ((obj org-gtd-transient--value))
  (with-slots (value toggle-set multi-value allow-empty choices) obj
    (if (and value toggle-set transient--prefix)
        (oset obj value nil)
      (let* ((overriding-terminal-local-map nil)
             (reader (oref obj reader))
             (prompt (transient-prompt obj))
             (value-str
              (if multi-value
                (mapconcat (lambda (v) (format "%s" v)) value ",") (format "%s" value)))
             (history-key (or (oref obj history-key)
                              (oref obj command)))
             (transient--history (alist-get history-key transient-history))
             (transient--history (if (or (null value-str)
                                         (eq value-str (car transient--history)))
                                     transient--history
                                   (cons value-str transient--history)))
             (initial-input (and transient-read-with-initial-input
                                 (car transient--history)))
             (history (if initial-input
                          (cons 'transient--history 1)
                        'transient--history))
             (value
              (cond
               (reader (funcall reader prompt initial-input history))
               (multi-value
                (completing-read-multiple prompt choices nil nil
                                          initial-input history))
               (choices
                (completing-read prompt choices nil t initial-input history))
               (t (read-string prompt initial-input history)))))
        (cond ((and (equal value "") (not allow-empty))
               (setq value nil))
              ((and (equal value "\"\"") allow-empty)
               (setq value "")))
        (when value
          (when (bound-and-true-p ivy-mode)
            (set-text-properties 0 (length (car transient--history)) nil
                                 (car transient--history)))
          (setf (alist-get history-key transient-history)
                (delete-dups transient--history)))
        value))))


;;; Set


(cl-defmethod transient-infix-set ((obj org-gtd-transient--display) value &optional tactic)
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

(cl-defmethod transient-infix-set ((obj org-gtd-transient--value) value)
  (let ((old-val (oref obj value)))
    (if (eq (oref obj combine-tactic) 'replace)
        ;; replace tactic: entire value gets replaced
        (oset obj value value)
      ;; merge tactic
      (if (oref obj multi-value)
          ;; value is a list (multi-value), so we concatenate
          (oset obj value (-concat old-val value))
        ;; value is not a list, so we append (merge tactic forces the result to be a list)
        (oset obj value (-snoc old-val value))))))


;;; Draw


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
  (let ((val (oref obj value)))
    (propertize (format "%s" val) 'face (if val 'transient-value 'transient-inactive-value))))

(cl-defmethod transient-format-description ((obj org-gtd-transient--value))
  (oref obj description))

(cl-defmethod transient-format-value ((obj org-gtd-transient--value))
  (let ((propertize-value (lambda (v) (propertize (format "%s" v) 'face 'transient-value))))
    (if-let ((value (oref obj value)))
        (if (oref obj multi-value)
            (if (cdr value)
                (mapconcat (lambda (v) (concat "\n     " (funcall propertize-value v))) value "")
              (funcall propertize-value (car value)))
          (funcall propertize-value (car (split-string (format "%s" value) "\n"))))
      (propertize "unset" 'face 'transient-inactive-value))))


(provide 'org-gtd)
;;; org-gtd.el ends here
