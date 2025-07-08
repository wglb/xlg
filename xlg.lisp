;;; File: xlg.lisp
;;; Description: Contains the core logic for the XLog logging library,
;;; including the WITH-OPEN-LOG-FILES and XLG macros.
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings
(in-package #:xlg) ; Renamed in-package form

;;; Macro: XLG
;;; Purpose: Writes a formatted log entry to a specified log stream.
;;; It checks if the stream variable is bound and is an actual stream before writing.
;;;
;;; Usage: (xlg stream-variable format-string &rest args)
;;;   - stream-variable: The actual variable (e.g., `app-log`, `error-log`) that has been
;;;     bound to an output stream by the `WITH-OPEN-LOG-FILES` macro.
;;;   - format-string: A standard Common Lisp format control string (e.g., "~a ~s").
;;;   - args: Zero or more arguments that will be formatted according to `format-string`.
(defmacro xlg (stream-variable format-string &rest args)
  "Logs a formatted message to the specified log stream.
   The stream must be bound by WITH-OPEN-LOG-FILES.
   Adds a newline character after each log entry and flushes the stream."
  ;; Now directly use stream-variable, which will be the lexically bound stream.
  `(when (streamp ,stream-variable) ; Check if the value of the variable is a stream
     (format ,stream-variable ,format-string ,@args) ; Write the formatted string
     (terpri ,stream-variable)                      ; Write a newline character
     (finish-output ,stream-variable)))           ; Explicitly flush the stream to ensure content is written
