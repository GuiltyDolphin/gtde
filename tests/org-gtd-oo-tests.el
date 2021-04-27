;;; org-gtd-oo-tests.el --- Tests for org-gtd-oo.el -*- lexical-binding: t -*-
;;; Code:


(require 'dash)
(require 'org-gtd-oo)


(defun org-gtd-test--find-test-case-file (basename)
  "Find the test case file path for BASENAME."
  (format "tests/cases/%s" basename))

(defun org-gtd--map-class-leaves (fun classes)
  "Map FUN over leaf classes of CLASSES.

CLASSES is traversed left-to-right, including children."
  (unless (null classes)
    (let* ((current (car classes))
           (right1 (org-gtd--map-class-leaves fun (eieio-class-children current)))
           (right2 (org-gtd--map-class-leaves fun (cdr classes))))
      (if (null right1)
          (cons (funcall fun current) right2)
        (-concat right1 right2)))))

(defmacro org-gtd-tests--should-error-with-match (form regex)
  "Evaluate FORM and check that it signals an error whose description matches REGEX."
  (declare (indent 1))
  `(should (string-match-p ,regex (cadr (should-error ,form)))))


(defmacro org-gtd-test--with-temp-file (prefix suffix text fvar &rest body)
  "Create a temporary file and execute BODY like `progn', with FVAR bound to the file name.

PREFIX is the prefix used for the filename, and likewise SUFFIX is the suffix.

TEXT is inserted automatically into the file."
  (declare (indent 4) (debug t))
  (let ((temp-file (make-symbol "temp-file"))
        (buffers-for-temp-file (make-symbol "buffers-for-temp-file")))
    `(let ((,temp-file (make-temp-file ,prefix nil ,suffix ,text)))
       (unwind-protect
           (let ((,fvar ,temp-file)) (progn ,@body))
         (let ((,buffers-for-temp-file (-filter (lambda (buffer) (equal (buffer-file-name buffer) ,temp-file)) (buffer-list))))
           (-each ,buffers-for-temp-file (lambda (buffer)
                                           (with-current-buffer buffer
                                             (set-buffer-modified-p nil)
                                             (kill-buffer buffer)))))
         (delete-file ,temp-file)))))

(defmacro org-gtd-test--with-temp-org-file (prefix text fvar &rest body)
  "Create a temporary file with prefix PREFIX and execute BODY like `progn', with FVAR bound to the name of the temporary file.

TEXT is inserted into the new file."
  (declare (indent 3) (debug t))
  `(org-gtd-test--with-temp-file ,prefix ".org" ,text ,fvar ,@body))

(defun org-gtd-test--find-item-by-id-in-file (id file)
  "Find the item with given ID in FILE."
  (let ((db (org-gtd--build-db-from-files (list file))))
    (org-gtd--db-get-entry db id)))


;;;;;;;;;;;;;;;;;
;;;;; Tests ;;;;;
;;;;;;;;;;;;;;;;;


(ert-deftest org-gtd-oo-test:base ()
  "Tests for `org-gtd--base'."
  ;; class should be abstract, we should not be able to make an instance
  (org-gtd-tests--should-error-with-match (org-gtd--base) "is abstract$"))

(ert-deftest org-gtd-oo-test:org-gtd--from-org:all-nonabstract-leaves-have-type ()
  "All non-abstract leaf classes of `org-gtd--from-org' must have a non-NIL default value set for the type-name property."
  (let ((leaves-with-type-name (org-gtd--map-class-leaves (lambda (c) (cons c (org-gtd--oref-default-or-nil c type-name))) '(org-gtd--from-org))))
    (--each leaves-with-type-name (-let (((c . k) it)) (should-not (equal (cons c nil) (cons c k)))))))

(ert-deftest org-gtd-oo-test:org-gtd--build-db-from-files ()
  "Tests for `org-gtd--build-db-from-files'."
  (let ((db (org-gtd--build-db-from-files (list (org-gtd-test--find-test-case-file "01-simple.org")))))

    ;; correct IDs should be parsed into items
    (should (equal (-sort #'string-lessp (hash-table-keys (oref db table)))
                   '("02-project" "03-action-standalone" "04-action-with-project" "05-waiting-for-with-project")))
    (let ((config (oref db global-config))
          (project (org-gtd--db-get-entry db "02-project"))
          (action-standalone (org-gtd--db-get-entry db "03-action-standalone"))
          (action-with-project (org-gtd--db-get-entry db "04-action-with-project"))
          (waiting-for-with-project (org-gtd--db-get-entry db "05-waiting-for-with-project")))

      ;; testing config
      (should (equal 'org-gtd--config (eieio-object-class config)))
      (should (equal (list (org-gtd--project-status :display "ACTIVE")
                           (org-gtd--project-status :display "COMPLETE")
                           (org-gtd--project-status :display "CANCELLED"))
                     (oref config statuses)))

      ;; testing project
      (should (equal 'org-gtd--project (eieio-object-class project)))
      (should (equal "02-project" (oref project id)))
      (should (equal "Test project" (oref project title)))
      (should (equal (org-gtd--project-status :display "COMPLETE") (oref project status)))

      ;; testing action-standalone
      (should (equal 'org-gtd--next-action (eieio-object-class action-standalone)))
      (should (equal "03-action-standalone" (oref action-standalone id)))
      (should (equal "A test standalone action" (oref action-standalone title)))

      ;; testing action-with-project
      (should (equal 'org-gtd--next-action (eieio-object-class action-with-project)))
      (should (equal "04-action-with-project" (oref action-with-project id)))
      (should (equal '("02-project") (oref action-with-project superior-projects)))
      (should (equal "Action of \"a test project\"" (oref action-with-project title)))
      (should (equal (org-gtd--some (list (org-gtd--context :name "test_context"))) (oref action-with-project context)))

      ;; testing waiting-for-with-project
      (should (equal 'org-gtd--waiting-for (eieio-object-class waiting-for-with-project)))
      (should (equal "05-waiting-for-with-project" (oref waiting-for-with-project id)))
      (should (equal '("02-project") (oref waiting-for-with-project superior-projects)))
      (should (equal "Waiting for of \"a test project\"" (oref waiting-for-with-project title)))))

  ;; unsupported GTD type
  (should (equal "something_unsupported"
                 (cdr (should-error (org-gtd--build-db-from-files (list (org-gtd-test--find-test-case-file "02-bad.org"))) :type 'org-gtd--unsupported-gtd-type)))))

(ert-deftest org-gtd-oo-test:parse-from-org ()
  "Tests for `org-gtd--parse-from-org'."
  (let ((config1 (org-gtd--config :statuses (list (org-gtd--project-status :display "ACTIVE")) :context-tag-regex "@\\(.*\\)"))
        (config2 (org-gtd--config :statuses (list (org-gtd--project-status :display "ACTIVE")) :context-tag-regex "@.*")))
    (should (equal (org-gtd--context :name "test") (org-gtd--parse-from-org #'org-gtd--context config1 "@test")))
    (should (equal (org-gtd--context :name "@test") (org-gtd--parse-from-org #'org-gtd--context config2 "@test")))))

(ert-deftest org-gtd-oo-test:write-item-to-file ()
  "Tests for `org-gtd--write-item-to-file'."
  (let ((case-text (concat "* Test config\n"
                              ":PROPERTIES:\n"
                              ":ORG_GTD_IS_CONFIG: t\n"
                              ":ORG_GTD_PROJECT_STATUSES: ACTIVE | INACTIVE\n"
                              ":ORG_GTD_CONTEXT_TAG_REGEX: @\\(.*\\)\n"
                              ":END:\n"
                              "* Test action\n"
                              ":PROPERTIES:\n"
                              ":ID: 01-test-action\n"
                              ":ORG_GTD_TYPE: next_action\n"
                              ":END:\n"
                              "* Test project\n"
                              ":PROPERTIES:\n"
                              ":ID: 01-test-project\n"
                              ":ORG_GTD_TYPE: project\n"
                              ":STATUS: ACTIVE\n"
                              ":END:"))
        (example-action (org-gtd--next-action :title "Modified action title" :id "01-test-action"))
        (example-project (org-gtd--project :title "Modified title" :id "01-test-project" :status (org-gtd--project-status :display "INACTIVE"))))
    (org-gtd-test--with-temp-org-file "test-file" case-text fvar
      (org-gtd--write-item-to-file fvar example-project)
      (org-gtd--write-item-to-file fvar example-action)
      (should (equal example-project (org-gtd-test--find-item-by-id-in-file "01-test-project" fvar)))
      (should (equal example-action (org-gtd-test--find-item-by-id-in-file "01-test-action" fvar))))))


(provide 'org-gtd-oo-tests)
;;; org-gtd-oo-tests.el ends here
