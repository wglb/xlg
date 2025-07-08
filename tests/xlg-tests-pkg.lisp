;;; File: tests/package.lisp
;;; Description: Defines the Common Lisp package for the XLog test suite.
;;; This file sets up the namespace and exports the test runner function.
(defpackage #:xlg-tests
  (:use #:cl #:fiveam #:xlg) ; Use standard CL, FiveAM, and the XLog library
  (:export #:run-xlg-tests)) ; Export the main function to run all tests

