;;; File: xlg-lib.asd
;;; Description: ASDF system definition for the XLog logging library.
;;; This file defines how the Common Lisp system is built and loaded.
(asdf:defsystem #:xlg-lib ; System name: :xlg-lib
  :description "A simple logging library for Common Lisp with multiple log streams."
  :author "Bill <wgl@ciex-security.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t ; Specifies that components are loaded in the order they appear
  :components ((:file "xlg-lib-pkg") ; Package file: xlg-lib-pkg.lisp
               (:file "xlg-lib")))   ; Main library file: xlg-lib.lisp
