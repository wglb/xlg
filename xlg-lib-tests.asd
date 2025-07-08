;;; File: xlg-lib-tests.asd
;;; Description: ASDF system definition for the XLog test suite.
;;; This file defines how the Common Lisp test system is built and run.
(asdf:defsystem #:xlg-lib-tests ; System name changed to :xlg-lib-tests
  :description "Regression tests for the XLog logging library."
  :author "Bill <wgl@ciex-security.com>" ; Personalized for Bill
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:xlg-lib      ; Dependency changed to :xlg-lib
               #:fiveam     ; The testing framework
               #:uiop)      ; For uiop:symbol-call in the perform method
  :perform (asdf:test-op (o s)
             ;; Calls the RUN-XLG-TESTS function defined in tests/suite.lisp
             (uiop:symbol-call '#:xlg-lib-tests '#:run-xlg-tests)) ; Package name changed
  :pathname "tests/" ; Specifies that test files are in the 'tests/' subdirectory
  :serial t           ; Components are loaded in order
  :components ((:file "xlg-lib-tests-pkg") ; Package file renamed
               (:file "xlg-lib-tests-suite"))) ; Test suite file renamed
