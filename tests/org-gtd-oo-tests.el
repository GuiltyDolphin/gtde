;;; org-gtd-oo-tests.el --- Tests for org-gtd-oo.el -*- lexical-binding: t -*-
;;; Code:


(require 'org-gtd-oo)


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


(provide 'org-gtd-oo-tests)
;;; org-gtd-oo-tests.el ends here
