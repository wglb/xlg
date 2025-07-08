;;; File: xlg-lib.asd
;;; Description: ASDF system definition for the XLog logging library.
;;; This file defines how the Common Lisp system is built and loaded.
(asdf:defsystem #:xlg-lib ; System name changed to :xlg-lib
  :description "A simple logging library for Common Lisp with multiple log streams."
  :author "Bill <wgl@ciex-security.com>" ; Personalized for Bill with updated email
  :license "MIT"
  :version "0.1.0"
  :serial t ; Specifies that components are loaded in the order they appear
  :components ((:file "xlg-lib-pkg") ; Package file renamed
               (:file "xlg-lib")))   ; Main logging macros file renamed
