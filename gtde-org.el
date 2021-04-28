;;; gtde-org.el --- gtde Org-specific definitions

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
;;; Defines gtde definitions for working with Org.
;;;
;;; Code:


(require 'gtde-oo)
(require 'org-id)


;;;;;;;;;;;;;;;;;;;
;;;;; Helpers ;;;;;
;;;;;;;;;;;;;;;;;;;


(defmacro gtde--with-visiting-org-marker (marker &rest body)
  "Execute BODY with MARKER position active for editing.

This ensures that the edit is performed in Org mode."
  (declare (indent 1) (debug t))
  `(gtde--with-visiting-marker ,marker
     (org-mode)
     (progn ,@body)))


;;;;;;;;;;;;;;;;;;;
;;;;; Methods ;;;;;
;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;; Reading ;;
;;;;;;;;;;;;;


(cl-defmethod gtde--parse-entry-properties ((pt (eql org)) (obj (subclass gtde--item)) config args props)
  (cl-call-next-method pt obj config (-concat args (list
                                                    :id (gtde--alist-get "ID" props)
                                                    :title (gtde--alist-get "ITEM" props)))
                       props))

(cl-defmethod gtde--parse-entry-properties ((pt (eql org)) (obj (subclass gtde--next-action)) config args props)
  (let ((tags-string (gtde--alist-get "TAGS" props)))
    (let ((contexts
           (if tags-string
               (let* ((tags (gtde--tag-string-to-list tags-string))
                      (context-re (oref config context-tag-regex))
                      (context-tags (--filter (string-match-p context-re it) tags)))
                 (gtde--some (--map (gtde--parse-from-raw pt #'gtde--context config it) context-tags)))
             (gtde--none))))
      (cl-call-next-method pt obj config (-concat args (list :context contexts)) props))))

(cl-defmethod gtde--parse-entry-properties ((pt (eql org)) (obj (subclass gtde--has-parent-projects)) config args props)
  (cl-call-next-method pt obj config (-concat args (list :projects (let ((projects-string (gtde--alist-get "GTDE_PROJECTS" props))) (and projects-string (read projects-string))))) props))

(cl-defmethod gtde--parse-entry-properties ((pt (eql org)) (obj (subclass gtde--project)) config args props)
  (cl-call-next-method pt obj config (-concat args (list :status (let ((status-string (gtde--alist-get "GTDE_STATUS" props)))
                                                         (and status-string (gtde--parse-from-raw pt #'gtde--project-status config status-string))))) props))

(cl-defmethod gtde--parse-entry-properties-no-config ((pt (eql org)) (obj (subclass gtde--config)) args props)
  (let* ((status-raw (gtde--alist-get "GTDE_PROJECT_STATUSES" props))
         (statuses (mapcar (-partial #'gtde--project-status :display) (split-string status-raw "[ |]" t)))
         (context-tag-re (gtde--alist-get "GTDE_CONTEXT_TAG_REGEX" props)))
  (cl-call-next-method pt obj (-concat args (list :statuses statuses :context-tag-regex context-tag-re))
                       props)))

(cl-defmethod gtde--build-db-from-files ((pt (eql org)) files)
  "Build a PT database from FILES."
  (let ((configs (org-map-entries (lambda () (apply #'gtde-definst #'gtde--config (gtde--parse-entry-properties-no-config pt #'gtde--config nil (org-entry-properties)))) "GTDE_IS_CONFIG=\"t\"" files)))
    (let* ((config (car configs))
           (db (gtde--new-db config)))
      (org-map-entries
       (lambda ()
         (let* ((properties (org-entry-properties))
                (id (gtde--alist-get "ID" properties))
                (type (gtde--alist-get "GTDE_TYPE" properties))
                (class-for-parsing (gtde--get-class-for-parsing type)))
           (let ((entry-to-add
                  (if class-for-parsing (apply #'gtde-definst class-for-parsing (gtde--parse-entry-properties pt class-for-parsing config nil properties))
                    (when type (signal 'gtde--unsupported-gtd-type type)))))
             (when entry-to-add (gtde--db-add-entry db id entry-to-add)))))
       nil files)
      db)))


;;;;;;;;;;;;;;;
;; Rendering ;;
;;;;;;;;;;;;;;;


(cl-defmethod gtde--write-item-to-file ((pt (eql org)) file item)
  "Write the given ITEM to the given FILE in Org PT."
  (let ((pos (org-id-find-id-in-file (oref item id) file t)))
    (if pos
        (gtde--with-visiting-org-marker pos
          (gtde--write-to-org item)
          (save-buffer))
      (error "Could not find entry %s" (oref item id)))))

(cl-defgeneric gtde--write-to-org (obj)
  "Write OBJ to the current Org entry.")

(cl-defmethod gtde--write-to-org ((obj gtde--item))
  (org-edit-headline (oref obj title)))

(cl-defmethod gtde--write-to-org ((obj gtde--project))
  (org-set-property "GTDE_STATUS" (gtde--render 'org (oref obj status)))
  (cl-call-next-method obj))


(provide 'gtde-org)
;;; gtde-org.el ends here
