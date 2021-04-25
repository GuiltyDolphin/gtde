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


;;;;;;;;;;;;;;;;;
;;;;; Tests ;;;;;
;;;;;;;;;;;;;;;;;


(ert-deftest org-gtd-oo-test:base ()
  "Tests for `org-gtd--base'."
  ;; class should be abstract, we should not be able to make an instance
  ;; the instance should be a monitor
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

      ;; testing waiting-for-with-project
      (should (equal 'org-gtd--waiting-for (eieio-object-class waiting-for-with-project)))
      (should (equal "05-waiting-for-with-project" (oref waiting-for-with-project id)))
      (should (equal '("02-project") (oref waiting-for-with-project superior-projects)))
      (should (equal "Waiting for of \"a test project\"" (oref waiting-for-with-project title)))))

  ;; unsupported GTD type
  (should (equal "something_unsupported"
                 (cdr (should-error (org-gtd--build-db-from-files (list (org-gtd-test--find-test-case-file "02-bad.org"))) :type 'org-gtd--unsupported-gtd-type)))))


(provide 'org-gtd-oo-tests)
;;; org-gtd-oo-tests.el ends here
