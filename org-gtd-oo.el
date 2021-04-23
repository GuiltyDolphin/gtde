;;; org-gtd-oo.el --- org-gtd object-oriented interface.

;; Copyright (C) 2021 Ben Moon
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
;;; Defines org-gtd classes.
;;;
;;; Code:

(require 'eieio)
(require 'org)


;;;;;;;;;;;;;;;;;;;
;;;;; Helpers ;;;;;
;;;;;;;;;;;;;;;;;;;


(defmacro org-gtd--type--list-of (p)
  "A list, where each element satisfies P."
  `(lambda (x) (and (listp x) (-all? ,p x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Interfaces ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass org-gtd--base ()
  ()
  :abstract t
  :documentation "Abstract base class for org-gtd.")

(defclass org-gtd--item (org-gtd--base)
  ((title :initarg :title
          :documentation "Title of the item.")
   (id :initarg :id
       :documentation "Unique identifier for the item. Don't change this manually."))
  :abstract t
  :documentation "Abstract class for item-like classes.")

(defclass org-gtd--has-parent-projects (org-gtd--base)
  ((superior-projects :initarg :projects
                      :documentation "Projects that contain this item."))
  :abstract t
  :documentation "Abstract class for entries that can have associated parent projects.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Configuration ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass org-gtd--project-status (org-gtd--base)
  ((display :initarg :display
            :type stringp
            :documentation "Textual display of the status."))
  :documentation "Status of a project.")

(defconst org-gtd--project-status--active
  (org-gtd--project-status :display "ACTIVE"))
(defconst org-gtd--project-status--cancelled
  (org-gtd--project-status :display "CANCELLED"))
(defconst org-gtd--project-status--complete
  (org-gtd--project-status :display "COMPLETE"))

(defclass org-gtd--config (org-gtd--base)
  ((statuses :initarg :statuses
             ;; NOTE: there are nicer error messages if you use `org-gtd-defist' with `org-gtd--validate-option-type'. (2021-04-23)
             :type (satisfies (lambda (x) (apply (org-gtd--type--list-of #'org-gtd--project-status-p) (list x))))
             :documentation "List of possible project statuses."))
  :documentation "Configuration for a GTD setup.")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - GTD ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass org-gtd--project (org-gtd--item)
  ((actions :initarg :actions
            :initform nil
            :documentation "Next actions associated with the project.")
   (status :initarg :status
           :documentation "Status of the project."
           :type org-gtd--project-status)
   (subprojects :documentation "References to subprojects."))
  :documentation "A project.")

(defclass org-gtd--next-action (org-gtd--item org-gtd--has-parent-projects)
  ((context :initarg :context
            :initform nil
            :documentation "Contexts required to be able to perform the action."))
  :documentation "A next action.")

(defclass org-gtd--waiting-for (org-gtd--item org-gtd--has-parent-projects)
  ((scheduled :initarg :scheduled
              :initform nil
              :documentation "Date scheduled to chase up the waiting for."))
  :documentation "An item waiting for someone else.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Database ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass org-gtd--db (org-gtd--base)
  ((global-config :initarg :global-config
                  :description "Global configuration used at base level."
                  :required t
                  :type org-gtd--config)
   (table :initarg :table
          :description "Table of IDs to items."))
  :description "Database for interacting with GTD items.")

(defun org-gtd--new-db (config)
  "Create a new database with CONFIG as the global configuration."
  (org-gtd-definst #'org-gtd--db :global-config config :table (make-hash-table :test #'equal)))

(defun org-gtd--db-get-entry (db id)
  "Retrieve the entry with id ID from DB."
  (gethash id (oref db table)))

(defun org-gtd--db-add-entry (db id entry)
  "Add ENTRY to database DB at key ID."
  (puthash id entry (oref db table)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Class methods ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;
;; Setup ;;
;;;;;;;;;;;


(define-error 'org-gtd--missing-required-option
  "Missing required option(s)")

(defun org-gtd--validate-option-type (obj opt pred)
  "Validate that OBJ's OPT option satisfies PRED, and fail otherwise."
  (unless (funcall pred (slot-value obj opt))
    (signal 'org-gtd--error--bad-type (list opt pred))))

(defun org-gtd--validate-required-options (obj props)
  "Check that OBJ provides each option in PROPS, fail otherwise."
  (let ((missing-opts))
    (dolist (prop props)
      (unless (slot-boundp obj prop)
        (push prop missing-opts)))
    (unless (null missing-opts)
      (signal 'org-gtd--missing-required-option (nreverse missing-opts)))))

(cl-defgeneric org-gtd--init (obj)
  "Initialize OBJ.

This method is called when an instance is created with
`org-gtd-definst', so it's a good place to put any
validation (e.g., checking for missing options) and
initialization you want to apply to all new instances.

You should usually either combine this method with `:before' or
`:after' (see `cl-defmethod'), or call `cl-call-next-method' in
the body.")

(cl-defmethod org-gtd--init ((obj org-gtd--base))
  "No-op.")

(cl-defmethod org-gtd--init :before ((obj org-gtd--config))
  ":statuses must be a list of statuses, and must be specified."
  (org-gtd--validate-required-options obj '(:statuses))
  (org-gtd--validate-option-type obj :statuses (org-gtd--type--list-of #'org-gtd--project-status-p)))

(cl-defmethod org-gtd--init ((obj org-gtd--db))
  "We require the `:global-config' and `:table' arguments to be bound."
  (org-gtd--validate-required-options obj '(:global-config :table)))

(defun org-gtd-definst (class &rest args)
  "Define an instance of CLASS using the given ARGS."
  (let ((inst (apply class args)))
    (org-gtd--init inst)
    inst))


(provide 'org-gtd-oo)
;;; org-gtd-oo.el ends here
