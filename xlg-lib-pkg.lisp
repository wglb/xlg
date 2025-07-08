;;; File: xlg-lib-pkg.lisp
;;; Description: Defines the Common Lisp package for the XLog library.
;;; This file sets up the namespace and exports the public functions/macros.
(defpackage #:xlg-lib ; Package name changed to :xlg-lib
  (:use #:cl) ; Use standard Common Lisp symbols
  (:export #:with-open-log-files ; Export the main macro for opening log files
           #:xlg                 ; Export the macro for writing log entries
           #:flush-all-log-streams)) ; Export the new function for flushing all log streams
