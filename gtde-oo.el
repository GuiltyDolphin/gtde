;;; gtde-oo.el --- gtde object-oriented interface.

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
;;; Defines gtde classes.
;;;
;;; Code:

(require 'eieio)


;;;;;;;;;;;;;;;;;;;
;;;;; Helpers ;;;;;
;;;;;;;;;;;;;;;;;;;


(defmacro gtde--type--list-of (p)
  "A list, where each element satisfies P."
  `(lambda (x) (and (listp x) (-all? ,p x))))

(defun gtde--alist-get (key alist)
  "Get the value at KEY in ALIST, comparing using `equal'."
  (alist-get key alist nil nil #'equal))

(defun gtde--map-class-ltr-depth-first (fun classes)
  "Map FUN over CLASSES and their subclasses.

CLASSES is traversed left-to-right, depth-first, thus the (application to the) most specific, left-most class will be first in the resulting list."
  (unless (null classes)
    (let* ((current (car classes))
           (left (gtde--map-class-ltr-depth-first fun (eieio-class-children current)))
           (middle (funcall fun current))
           (right (gtde--map-class-ltr-depth-first fun (cdr classes))))
      (-concat left (list middle) right))))

(defun gtde--first-class-ltr-depth-first (pred classes)
  "Return the first class from CLASSES (or a subclass) that satisfies PRED.

CLASSES is traversed left-to-right, depth-first, thus the most specific, left-most class that matches will be returned."
  (-first pred (gtde--map-class-ltr-depth-first #'identity classes)))

(defmacro gtde--oref-default-or-nil (obj slot)
  "Get the default value of OBJ (maybe a class) for SLOT.

Return NIL if the slot is unbound."
  `(when (slot-boundp ,obj ',slot) (oref-default ,obj ,slot)))

(defmacro gtde--with-visiting-marker (marker &rest body)
  "Execute BODY with MARKER position active for editing."
  (declare (indent 1) (debug t))
  `(with-current-buffer
     (set-buffer (marker-buffer ,marker))
     (goto-char (marker-position ,marker))
     (progn ,@body)))

(defun gtde--tag-string-to-list (org-tag-string)
  "Parse ORG-TAG-STRING into a list of tags."
  (save-match-data
    (split-string org-tag-string ":" t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Helpers ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass gtde--maybe ()
  ((value :initarg :value
          :documentation "Underlying value."))
  :documentation "A value that may or may not be present.")

(defun gtde--none ()
  "Nothing."
  (gtde--maybe))

(defun gtde--some (value)
  "Return VALUE as a unit of `gtde--maybe'."
  (gtde--maybe :value value))

(defun gtde--on-maybe (value noneval somefun)
  "If VALUE is not present, then return NONEVAL, else execute SOMEFUN with the embedded value."
  (if (equal value (gtde--none))
      noneval
    (funcall somefun (slot-value value 'value))))

(defmacro gtde--type-maybe-of (p)
  "A value satisfying P wrapped in `gtde--maybe'."
  `(lambda (x) (and (gtde--maybe-p x) (gtde--on-maybe x t ,p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Interfaces ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass gtde--base ()
  ()
  :abstract t
  :documentation "Abstract base class for gtde.")

(defclass gtde--item (gtde--base)
  ((title :initarg :title
          :documentation "Title of the item.")
   (id :initarg :id
       :documentation "Unique identifier for the item. Don't change this manually."))
  :abstract t
  :documentation "Abstract class for item-like classes.")

(defclass gtde--has-parent-projects (gtde--base)
  ((superior-projects :initarg :projects
                      :initform nil
                      :documentation "Projects that contain this item."))
  :abstract t
  :documentation "Abstract class for entries that can have associated parent projects.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Configuration ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass gtde--project-status (gtde--base)
  ((display :initarg :display
            :type stringp
            :documentation "Textual display of the status."))
  :documentation "Status of a project.")

(defclass gtde--config (gtde--base)
  ((statuses :initarg :statuses
             ;; NOTE: there are nicer error messages if you use `gtde-defist' with `gtde--validate-option-type'. (2021-04-23)
             :type (satisfies (lambda (x) (apply (gtde--type--list-of #'gtde--project-status-p) (list x))))
             :documentation "List of possible project statuses.")
   (context-tag-regex
    :initarg :context-tag-regex
    :type stringp
    :documentation "Regular expression used to match tags that represent contexts. If a match group is provided, it is used to extract the actual context text, otherwise the full match is used."))
  :documentation "Configuration for a GTD setup.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Parsing ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass gtde--from-org (gtde--base)
  ((type-name
    :required t
    :type stringp
    :documentation "Value used to identify whether an entry is of this class' type."))
  :abstract t
  :documentation "Interface for classes that can be parsed from Org entries.")

(defun gtde--get-class-for-parsing (type)
  "Get the class for parsing TYPE entries."
  (when type
    (gtde--first-class-ltr-depth-first (lambda (c) (equal (gtde--oref-default-or-nil c type-name) type)) '(gtde--from-org))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - GTD ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass gtde--project (gtde--item gtde--from-org)
  ((actions :initarg :actions
            :initform nil
            :documentation "Next actions associated with the project.")
   (status :initarg :status
           :documentation "Status of the project."
           :type gtde--project-status)
   (type-name :initform "project")
   (subprojects :documentation "References to subprojects."))
  :documentation "A project.")

(defclass gtde--context (gtde--base)
  ((name :initarg :name
         :type stringp
         :documentation "Text value of the context."))
  :documentation "Context of an action.")

(defclass gtde--next-action (gtde--item gtde--has-parent-projects gtde--from-org)
  ((context :initarg :context
            :initform (gtde--none)
            :documentation "Contexts required to be able to perform the action.")
   (type-name :initform "next_action"))
  :documentation "A next action.")

(defclass gtde--waiting-for (gtde--item gtde--has-parent-projects gtde--from-org)
  ((scheduled :initarg :scheduled
              :initform nil
              :documentation "Date scheduled to chase up the waiting for.")
   (type-name :initform "waiting_for"))
  :documentation "An item waiting for someone else.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Database ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass gtde--db (gtde--base)
  ((global-config :initarg :global-config
                  :description "Global configuration used at base level."
                  :required t
                  :type gtde--config)
   (table :initarg :table
          :description "Table of IDs to items."))
  :description "Database for interacting with GTD items.")

(defun gtde--new-db (config)
  "Create a new database with CONFIG as the global configuration."
  (gtde-definst #'gtde--db :global-config config :table (make-hash-table :test #'equal)))

(defun gtde--db-get-entry (db id)
  "Retrieve the entry with id ID from DB."
  (gethash id (oref db table)))

(defun gtde--db-add-entry (db id entry)
  "Add ENTRY to database DB at key ID."
  (puthash id entry (oref db table)))

(define-error 'gtde--unsupported-gtd-type
  "Unsupported GTD type")

(cl-defgeneric gtde--build-db-from-files (project-type files)
  "Build database for the given PROJECT-TYPE from FILES.")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Class methods ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;
;; Setup ;;
;;;;;;;;;;;


(define-error 'gtde--missing-required-option
  "Missing required option(s)")

(defun gtde--validate-option-type (obj opt pred)
  "Validate that OBJ's OPT option satisfies PRED, and fail otherwise."
  (unless (funcall pred (slot-value obj opt))
    (signal 'gtde--error--bad-type (list opt pred))))

(defun gtde--validate-required-options (obj props)
  "Check that OBJ provides each option in PROPS, fail otherwise."
  (let ((missing-opts))
    (dolist (prop props)
      (unless (slot-boundp obj prop)
        (push prop missing-opts)))
    (unless (null missing-opts)
      (signal 'gtde--missing-required-option (nreverse missing-opts)))))

(cl-defgeneric gtde--init (obj)
  "Initialize OBJ.

This method is called when an instance is created with
`gtde-definst', so it's a good place to put any
validation (e.g., checking for missing options) and
initialization you want to apply to all new instances.

You should usually either combine this method with `:before' or
`:after' (see `cl-defmethod'), or call `cl-call-next-method' in
the body.")

(cl-defmethod gtde--init ((obj gtde--base))
  "No-op.")

(cl-defmethod gtde--init :before ((obj gtde--config))
  ":statuses must be a list of statuses, and must be specified."
  (gtde--validate-required-options obj '(:statuses))
  (gtde--validate-option-type obj :statuses (gtde--type--list-of #'gtde--project-status-p)))

(cl-defmethod gtde--init ((obj gtde--db))
  "We require the `:global-config' and `:table' arguments to be bound."
  (gtde--validate-required-options obj '(:global-config :table)))

(cl-defmethod gtde--init :before ((obj gtde--project))
  ":status must be a status, and must be specified."
  (gtde--validate-required-options obj '(:status))
  (gtde--validate-option-type obj :status #'gtde--project-status-p))

(cl-defmethod gtde--init :before ((obj gtde--context))
  ":name must be specified, and must be a string."
  (gtde--validate-required-options obj '(:name))
  (gtde--validate-option-type obj :name #'stringp))

(cl-defmethod gtde--init :before ((obj gtde--next-action))
  ":context is optionally a list of contexts."
  (gtde--validate-required-options obj '(:context))
  (gtde--validate-option-type obj :context (gtde--type-maybe-of (gtde--type--list-of #'gtde--context-p))))

(defun gtde-definst (class &rest args)
  "Define an instance of CLASS using the given ARGS."
  (let ((inst (apply class args)))
    (gtde--init inst)
    inst))


;;;;;;;;;;;;;
;; Parsing ;;
;;;;;;;;;;;;;


(cl-defgeneric gtde--parse-from-raw (project-type class config text)
  "Parse an instance of CLASS from TEXT.

CONFIG is the available configuration context.
PROJECT-TYPE is the project type for parsing (e.g., org, JSON, etc.).")

(cl-defmethod gtde--parse-from-raw (_pt (_obj (subclass gtde--project-status)) _config text)
  (gtde-definst #'gtde--project-status :display text))

(cl-defmethod gtde--parse-from-raw (_pt (_obj (subclass gtde--context)) config text)
  (let* ((context-tag-re (oref config context-tag-regex))
         (context-text
          (save-match-data
            (string-match context-tag-re text)
            (or (match-string-no-properties 1 text) (match-string-no-properties 0 text)))))
    (gtde-definst #'gtde--context :name context-text)))

(cl-defgeneric gtde--get-prop (project-type prop-name props)
  "Get the property PROP-NAME from PROPS, a property dictionary for a PROJECT-TYPE setup.")

(cl-defgeneric gtde--parse-entry-properties (project-type obj config args props)
  "Specify how to parse PROPS as a specification of properties for OBJ.

Current specification is held in ARGS.
CONFIG holds the current global configuration.
PROJECT-TYPE is the type of the project (e.g., org, json, etc.).")

(cl-defmethod gtde--parse-entry-properties (_pt (_ (subclass gtde--base)) _config args _props)
  "When we hit the base class, we simply return ARGS as a base case."
  args)

(cl-defmethod gtde--parse-entry-properties (pt (obj (subclass gtde--has-parent-projects)) config args props)
  (cl-call-next-method pt obj config (-concat args (list :projects (let ((projects-string (gtde--get-prop pt "GTDE_PROJECTS" props))) (and projects-string (read projects-string))))) props))

(cl-defmethod gtde--parse-entry-properties (pt (obj (subclass gtde--project)) config args props)
  (cl-call-next-method pt obj config (-concat args (list :status (let ((status-string (gtde--get-prop pt "GTDE_STATUS" props)))
                                                         (and status-string (gtde--parse-from-raw pt #'gtde--project-status config status-string))))) props))

(cl-defgeneric gtde--parse-entry-properties-no-config (project-type obj args props)
  "Specify how to parse PROPS as a specification of properties for OBJ.

Current specification is held in ARGS.
PROJECT-TYPE is the current project type.")

(cl-defmethod gtde--parse-entry-properties-no-config (_pt (_ (subclass gtde--base)) args _props)
  "When we hit the base class, we simply return ARGS as a base case."
  args)


;;;;;;;;;;;;;;;
;; Rendering ;;
;;;;;;;;;;;;;;;


(cl-defgeneric gtde--render (project-type obj)
  "Render OBJ to text based on PROJECT-TYPE.")

(cl-defmethod gtde--render (_pt (obj gtde--project-status))
  (oref obj display))

(cl-defgeneric gtde--write-item-to-file (project-type file item)
  "Write the given ITEM to the given FILE with PROJECT-TYPE.")


(provide 'gtde-oo)
;;; gtde-oo.el ends here
