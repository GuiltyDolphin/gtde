;;; gtde-json.el --- gtde JSON-specific definitions

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
;;; Defines gtde definitions for working with JSON.
;;;
;;; Code:


(require 'gtde-oo)


;;;;;;;;;;;;;;;;;;;
;;;;; Methods ;;;;;
;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;; Reading ;;
;;;;;;;;;;;;;


(cl-defmethod gtde--get-prop ((_pt (eql json)) key props)
  "Lookup KEY in PROPS, which is a hash table for JSON projects."
  (gethash key props))

(cl-defmethod gtde--parse-entry-properties ((pt (eql json)) (obj (subclass gtde--item)) config args props)
  (cl-call-next-method pt obj config (-concat args (list
                                                    :id (gethash "id" props)
                                                    :title (gethash "title" props)))
                       props))

(cl-defmethod gtde--parse-entry-properties ((pt (eql json)) (obj (subclass gtde--next-action)) config args props)
  (let ((tags-string (gethash "TAGS" props)))
    (let ((contexts
           (if tags-string
               (let* ((tags (gtde--tag-string-to-list tags-string))
                      (context-re (oref config context-tag-regex))
                      (context-tags (--filter (string-match-p context-re it) tags)))
                 (gtde--some (--map (gtde--parse-from-raw pt #'gtde--context config it) context-tags)))
             (gtde--none))))
      (cl-call-next-method pt obj config (-concat args (list :context contexts)) props))))

(cl-defmethod gtde--parse-entry-properties-no-config ((pt (eql json)) (obj (subclass gtde--config)) args props)
  (let* ((status-raw (gethash "GTDE_PROJECT_STATUSES" props))
         (statuses (mapcar (-partial #'gtde--project-status :display) (split-string status-raw "[ |]" t)))
         (context-tag-re (gethash "GTDE_CONTEXT_TAG_REGEX" props)))
    (cl-call-next-method pt obj (-concat args (list :statuses statuses :context-tag-regex context-tag-re))
                       props)))

(defun gtde-json--read-file (file)
  "Read the first JSON object contained in FILE and return it as a hash."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'hash-table)))

(defun gtde-json--combined-keys (tables)
  "Return the combined keys of TABLES."
  (-uniq (-flatten-n 1 (-map #'hash-table-keys tables))))

(defun gtde-json--gethash-first (key tables)
  "Get the entry at the first occurence of KEY in TABLES."
  (-first-item (--map (gethash key it) tables)))

(cl-defmethod gtde--build-db-from-files ((pt (eql json)) files)
  "Build a PT database from FILES."
  (let* ((tables (-map #'gtde-json--read-file files))
         (config-entries (--filter (eq t (gethash "GTDE_IS_CONFIG" (gethash "properties" it))) (--map (gtde-json--gethash-first it tables) (gtde-json--combined-keys tables))))
         (configs (--map (apply #'gtde-definst #'gtde--config (gtde--parse-entry-properties-no-config pt #'gtde--config nil (gethash "properties" it))) config-entries)))
    (let* ((config (car configs))
           (db (gtde--new-db config)))
      (dolist (key (gtde-json--combined-keys tables))
        (let* ((entry (gtde-json--gethash-first key tables))
               (properties (gethash "properties" entry))
               (id (gethash "id" properties))
               (type (gethash "GTDE_TYPE" properties))
               (class-for-parsing (gtde--get-class-for-parsing type)))
          (let ((entry-to-add
                 (if class-for-parsing (apply #'gtde-definst class-for-parsing (gtde--parse-entry-properties pt class-for-parsing config nil properties))
                   (when type (signal 'gtde--unsupported-gtd-type type)))))
             (when entry-to-add (gtde--db-add-entry db id entry-to-add)))))
      db)))


;;;;;;;;;;;;;;;
;; Rendering ;;
;;;;;;;;;;;;;;;


(cl-defgeneric gtde-json--write-to-object (obj json)
  "Write OBJ to the current JSON object.")

(cl-defmethod gtde-json--write-to-object ((item gtde--item) json)
  (puthash "title" (oref item title) json))

(cl-defmethod gtde-json--write-to-object ((obj gtde--project) json)
  (puthash "GTDE_STATUS" (gtde--render 'json (oref obj status)) json)
  (cl-call-next-method obj json))

(cl-defmethod gtde--write-item-to-file ((pt (eql json)) file item)
  "Write the given ITEM to the given FILE in JSON PT."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((table (json-parse-buffer))
           (item-obj (gethash (oref item id) table)))
      (gtde-json--write-to-object item (gethash "properties" item-obj))
      (delete-region (point-min) (point-max))
      (json-insert table)
      (write-file file)
      )))


(provide 'gtde-json)
;;; gtde-json.el ends here
