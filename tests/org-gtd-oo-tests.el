;;; org-gtd-oo-tests.el --- Tests for org-gtd-oo.el -*- lexical-binding: t -*-
;;; Code:


(require 'org-gtd-oo)


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


(provide 'org-gtd-oo-tests)
;;; org-gtd-oo-tests.el ends here
