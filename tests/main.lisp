(defpackage cl-top/tests/main
  (:use :cl
        :cl-top
        :rove))
(in-package :cl-top/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-top)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
