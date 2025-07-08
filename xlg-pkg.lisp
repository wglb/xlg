;;;; package.lisp

(defpackage #:xlg
  (:use #:cl)
  (:export #:with-open-log-files ; Export the main macro for opening log files
           #:xlg))
