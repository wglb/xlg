;;; File: xlg-pkg.lisp
;;; Description: Defines the Common Lisp package for the XLog library.
;;; This file sets up the namespace and exports the public functions/macros.
(defpackage #:xlg-lib ; Corrected package name
  (:use #:cl) ; Use standard Common Lisp symbols
  (:export #:with-open-log-files ; Export the main macro for opening log files
           #:xlg                 ; Export the macro for writing log entries
           #:xlgt                ; Export the macro for logging to file and stdout
           #:flush-all-log-streams)) ; Export the new function for flushing all log streams
