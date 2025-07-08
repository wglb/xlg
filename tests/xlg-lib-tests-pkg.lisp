;;; File: tests/xlg-lib-tests-pkg.lisp
;;; Description: Defines the Common Lisp package for the XLog test suite.
;;; This file sets up the namespace and exports the test runner function.
(defpackage #:xlg-lib-tests ; This defines the package XLG-LIB-TESTS
  (:use #:cl #:fiveam #:xlg-lib) ; Use standard CL, FiveAM, and the XLog library (renamed)
  (:export #:run-xlg-tests)) ; Export the main function to run all tests
