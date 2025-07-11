;;; File: xlg-lib-pkg.lisp
;;; Description: Defines the Common Lisp package for the XLog library.
;;; This file sets up the namespace and exports the public functions/macros.
(defpackage #:xlg-lib ; Package name corrected to :xlg-lib
  (:use #:cl) ; Use standard Common Lisp symbols
  (:export #:with-open-log-files ; Export the main macro for opening log files
           #:xlg                ; Export the macro for writing log entries (file only)
           #:xlgt               ; Export the new macro for writing log entries (file and stdout)
           #:flush-all-log-streams ; Export the function to flush all streams
           #:dates-ymd          ; Export the date formatting utility
           #:formatted-current-time-micro)) ; Export the microsecond time formatting utility
