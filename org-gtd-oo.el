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
(require 'org-id)


;;;;;;;;;;;;;;;;;;;
;;;;; Helpers ;;;;;
;;;;;;;;;;;;;;;;;;;


(defmacro org-gtd--type--list-of (p)
  "A list, where each element satisfies P."
  `(lambda (x) (and (listp x) (-all? ,p x))))

(defun org-gtd--alist-get (key alist)
  "Get the value at KEY in ALIST, comparing using `equal'."
  (alist-get key alist nil nil #'equal))

(defun org-gtd--map-class-ltr-depth-first (fun classes)
  "Map FUN over CLASSES and their subclasses.

CLASSES is traversed left-to-right, depth-first, thus the (application to the) most specific, left-most class will be first in the resulting list."
  (unless (null classes)
    (let* ((current (car classes))
           (left (org-gtd--map-class-ltr-depth-first fun (eieio-class-children current)))
           (middle (funcall fun current))
           (right (org-gtd--map-class-ltr-depth-first fun (cdr classes))))
      (-concat left (list middle) right))))

(defun org-gtd--first-class-ltr-depth-first (pred classes)
  "Return the first class from CLASSES (or a subclass) that satisfies PRED.

CLASSES is traversed left-to-right, depth-first, thus the most specific, left-most class that matches will be returned."
  (-first pred (org-gtd--map-class-ltr-depth-first #'identity classes)))

(defmacro org-gtd--oref-default-or-nil (obj slot)
  "Get the default value of OBJ (maybe a class) for SLOT.

Return NIL if the slot is unbound."
  `(when (slot-boundp ,obj ',slot) (oref-default ,obj ,slot)))

(defmacro org-gtd--with-visiting-marker (marker &rest body)
  "Execute BODY with MARKER position active for editing."
  (declare (indent 1) (debug t))
  `(with-current-buffer
     (set-buffer (marker-buffer ,marker))
     (goto-char (marker-position ,marker))
     (progn ,@body)))

(defmacro org-gtd--with-visiting-org-marker (marker &rest body)
  "Execute BODY with MARKER position active for editing.

This ensures that the edit is performed in Org mode."
  (declare (indent 1) (debug t))
  `(org-gtd--with-visiting-marker ,marker
     (org-mode)
     (progn ,@body)))

(defun org-gtd--tag-string-to-list (org-tag-string)
  "Parse ORG-TAG-STRING into a list of tags."
  (save-match-data
    (split-string org-tag-string ":" t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Helpers ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass org-gtd--maybe ()
  ((value :initarg :value
          :documentation "Underlying value."))
  :documentation "A value that may or may not be present.")

(defun org-gtd--none ()
  "Nothing."
  (org-gtd--maybe))

(defun org-gtd--some (value)
  "Return VALUE as a unit of `org-gtd--maybe'."
  (org-gtd--maybe :value value))

(defun org-gtd--on-maybe (value noneval somefun)
  "If VALUE is not present, then return NONEVAL, else execute SOMEFUN with the embedded value."
  (if (equal value (org-gtd--none))
      noneval
    (funcall somefun (slot-value value 'value))))

(defmacro org-gtd--type-maybe-of (p)
  "A value satisfying P wrapped in `org-gtd--maybe'."
  `(lambda (x) (and (org-gtd--maybe-p x) (org-gtd--on-maybe x t ,p))))


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
                      :initform nil
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
             :documentation "List of possible project statuses.")
   (context-tag-regex
    :initarg :context-tag-regex
    :type stringp
    :documentation "Regular expression used to match tags that represent contexts. If a match group is provided, it is used to extract the actual context text, otherwise the full match is used."))
  :documentation "Configuration for a GTD setup.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Parsing ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass org-gtd--from-org (org-gtd--base)
  ((type-name
    :required t
    :type stringp
    :documentation "Value used to identify whether an entry is of this class' type."))
  :abstract t
  :documentation "Interface for classes that can be parsed from Org entries.")

(defun org-gtd--get-class-for-parsing (type)
  "Get the class for parsing TYPE entries."
  (when type
    (org-gtd--first-class-ltr-depth-first (lambda (c) (equal (org-gtd--oref-default-or-nil c type-name) type)) '(org-gtd--from-org))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - GTD ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass org-gtd--project (org-gtd--item org-gtd--from-org)
  ((actions :initarg :actions
            :initform nil
            :documentation "Next actions associated with the project.")
   (status :initarg :status
           :documentation "Status of the project."
           :type org-gtd--project-status)
   (type-name :initform "project")
   (subprojects :documentation "References to subprojects."))
  :documentation "A project.")

(defclass org-gtd--context (org-gtd--base)
  ((name :initarg :name
         :type stringp
         :documentation "Text value of the context."))
  :documentation "Context of an action.")

(defclass org-gtd--next-action (org-gtd--item org-gtd--has-parent-projects org-gtd--from-org)
  ((context :initarg :context
            :initform (org-gtd--none)
            :documentation "Contexts required to be able to perform the action.")
   (type-name :initform "next_action"))
  :documentation "A next action.")

(defclass org-gtd--waiting-for (org-gtd--item org-gtd--has-parent-projects org-gtd--from-org)
  ((scheduled :initarg :scheduled
              :initform nil
              :documentation "Date scheduled to chase up the waiting for.")
   (type-name :initform "waiting_for"))
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

(define-error 'org-gtd--unsupported-gtd-type
  "Not a valid value for ORG_GTD_TYPE")

(defun org-gtd--build-db-from-files (files)
  "Build a database from FILES."
  (let ((configs (org-map-entries (lambda () (apply #'org-gtd-definst #'org-gtd--config (org-gtd--parse-entry-properties-no-config #'org-gtd--config nil (org-entry-properties)))) "ORG_GTD_IS_CONFIG=\"t\"" files)))
    (let* ((config (car configs))
           (db (org-gtd--new-db config)))
      (org-map-entries
       (lambda ()
         (let* ((properties (org-entry-properties))
                (id (org-gtd--alist-get "ID" properties))
                (type (org-gtd--alist-get "ORG_GTD_TYPE" properties))
                (class-for-parsing (org-gtd--get-class-for-parsing type)))
           (let ((entry-to-add
                  (if class-for-parsing (apply #'org-gtd-definst class-for-parsing (org-gtd--parse-entry-properties class-for-parsing config nil properties))
                    (when type (signal 'org-gtd--unsupported-gtd-type type)))))
             (when entry-to-add (org-gtd--db-add-entry db id entry-to-add)))))
       nil files)
      db)))

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

(cl-defmethod org-gtd--init :before ((obj org-gtd--project))
  ":status must be a status, and must be specified."
  (org-gtd--validate-required-options obj '(:status))
  (org-gtd--validate-option-type obj :status #'org-gtd--project-status-p))

(cl-defmethod org-gtd--init :before ((obj org-gtd--context))
  ":name must be specified, and must be a string."
  (org-gtd--validate-required-options obj '(:name))
  (org-gtd--validate-option-type obj :name #'stringp))

(cl-defmethod org-gtd--init :before ((obj org-gtd--next-action))
  ":context is optionally a list of contexts."
  (org-gtd--validate-required-options obj '(:context))
  (org-gtd--validate-option-type obj :context (org-gtd--type-maybe-of (org-gtd--type--list-of #'org-gtd--context-p))))

(defun org-gtd-definst (class &rest args)
  "Define an instance of CLASS using the given ARGS."
  (let ((inst (apply class args)))
    (org-gtd--init inst)
    inst))


;;;;;;;;;;;;;
;; Parsing ;;
;;;;;;;;;;;;;


(cl-defgeneric org-gtd--parse-from-org (class config org-text)
  "Parse an instance of CLASS from ORG-TEXT, with global config CONFIG.")

(cl-defmethod org-gtd--parse-from-org ((_obj (subclass org-gtd--project-status)) _config org-text)
  (org-gtd-definst #'org-gtd--project-status :display org-text))

(cl-defmethod org-gtd--parse-from-org ((_obj (subclass org-gtd--context)) config org-text)
  (let* ((context-tag-re (oref config context-tag-regex))
         (context-text
          (save-match-data
            (string-match context-tag-re org-text)
            (or (match-string-no-properties 1 org-text) (match-string-no-properties 0 org-text)))))
    (org-gtd-definst #'org-gtd--context :name context-text)))

(cl-defgeneric org-gtd--parse-entry-properties (obj config args props)
  "Specify how to parse PROPS as a specification of properties for OBJ.

Current specification is held in ARGS.
CONFIG holds the current global configuration.")

(cl-defmethod org-gtd--parse-entry-properties ((_ (subclass org-gtd--base)) _config args _props)
  "When we hit the base class, we simply return ARGS as a base case."
  args)

(cl-defmethod org-gtd--parse-entry-properties ((obj (subclass org-gtd--item)) config args props)
  (cl-call-next-method obj config (-concat args (list
                                                 :id (org-gtd--alist-get "ID" props)
                                                 :title (org-gtd--alist-get "ITEM" props)))
                       props))

(cl-defmethod org-gtd--parse-entry-properties ((obj (subclass org-gtd--has-parent-projects)) config args props)
  (cl-call-next-method obj config (-concat args (list :projects (let ((projects-string (org-gtd--alist-get "ORG_GTD_PROJECTS" props))) (and projects-string (read projects-string))))) props))

(cl-defmethod org-gtd--parse-entry-properties ((obj (subclass org-gtd--project)) config args props)
  (cl-call-next-method obj config (-concat args (list :status (let ((status-string (org-gtd--alist-get "ORG_GTD_STATUS" props)))
                                                         (and status-string (org-gtd--parse-from-org #'org-gtd--project-status config status-string))))) props))

(cl-defmethod org-gtd--parse-entry-properties ((obj (subclass org-gtd--next-action)) config args props)
  (let ((tags-string (org-gtd--alist-get "TAGS" props)))
    (let ((contexts
           (if tags-string
               (let* ((tags (org-gtd--tag-string-to-list tags-string))
                      (context-re (oref config context-tag-regex))
                      (context-tags (--filter (string-match-p context-re it) tags)))
                 (org-gtd--some (--map (org-gtd--parse-from-org #'org-gtd--context config it) context-tags)))
             (org-gtd--none))))
      (cl-call-next-method obj config (-concat args (list :context contexts)) props))))

(cl-defgeneric org-gtd--parse-entry-properties-no-config (obj args props)
  "Specify how to parse PROPS as a specification of properties for OBJ.

Current specification is held in ARGS.")

(cl-defmethod org-gtd--parse-entry-properties-no-config ((_ (subclass org-gtd--base)) args _props)
  "When we hit the base class, we simply return ARGS as a base case."
  args)

(cl-defmethod org-gtd--parse-entry-properties-no-config ((obj (subclass org-gtd--config)) args props)
  (let* ((status-raw (org-gtd--alist-get "ORG_GTD_PROJECT_STATUSES" props))
         (statuses (mapcar (-partial #'org-gtd--project-status :display) (split-string status-raw "[ |]" t)))
         (context-tag-re (org-gtd--alist-get "ORG_GTD_CONTEXT_TAG_REGEX" props)))
  (cl-call-next-method obj (-concat args (list :statuses statuses :context-tag-regex context-tag-re))
                       props)))


;;;;;;;;;;;;;;;
;; Rendering ;;
;;;;;;;;;;;;;;;


(cl-defgeneric org-gtd--render-to-org (obj)
  "Render OBJ as Org text.")

(cl-defmethod org-gtd--render-to-org ((obj org-gtd--project-status))
  (oref obj display))

(cl-defgeneric org-gtd--write-to-org (obj)
  "Write OBJ to the current Org entry.")

(cl-defmethod org-gtd--write-to-org ((obj org-gtd--item))
  (org-edit-headline (oref obj title)))

(cl-defmethod org-gtd--write-to-org ((obj org-gtd--project))
  (org-set-property "ORG_GTD_STATUS" (org-gtd--render-to-org (oref obj status)))
  (cl-call-next-method obj))

(defun org-gtd--write-item-to-file (file item)
  "Write the given ITEM to the given FILE."
  (let ((pos (org-id-find-id-in-file (oref item id) file t)))
    (if pos
        (org-gtd--with-visiting-org-marker pos
          (org-gtd--write-to-org item)
          (save-buffer))
      (error "Could not find entry %s" (oref item id)))))


(provide 'org-gtd-oo)
;;; org-gtd-oo.el ends here
