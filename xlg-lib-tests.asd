;;; File: xlg-lib-tests.asd
;;; Description: ASDF system definition for the XLog test suite.
;;; This file defines how the Common Lisp test system is built and run.
(asdf:defsystem #:xlg-lib-tests ; System name: :xlg-lib-tests
  :description "Regression tests for the XLog logging library."
  :author "Bill <wgl@ciex-security.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:xlg-lib      ; Dependency on the main XLog library
               #:fiveam       ; The testing framework
               #:uiop)        ; For uiop:symbol-call in the perform method
  :perform (asdf:test-op (o s)
             ;; Calls the RUN-XLG-TESTS function defined in tests/xlg-lib-tests-suite.lisp
             (uiop:symbol-call '#:xlg-lib-tests '#:run-xlg-tests))
  :pathname "tests/" ; Specifies that test files are in the 'tests/' subdirectory
  :serial t           ; Components are loaded in order
  :components ((:file "xlg-lib-tests-pkg") ; Package file for tests: xlg-lib-tests-pkg.lisp
               (:file "xlg-lib-tests-suite"))) ; Test suite file: xlg-lib-tests-suite.lisp
