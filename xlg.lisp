;;; File: xlg.lisp
;;; Description: Contains the core logic for the XLog logging library,
;;; including the WITH-OPEN-LOG-FILES and XLG macros.
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings
(in-package #:xlg) ; Renamed in-package form

;;; Function: DATES-YMD
;;; Purpose: Creates a file name prefix based on the current date and time.
;;;
;;; Arguments:
;;;   dates: A keyword specifying the date/time format.
;;;          :hms - YYYY-MM-DD-HH-MM-SS_
;;;          :hour - YYYY-MM-DD-HH_
;;;          :ym - YYYY-MM_
;;;          :ymd - YYYY-MM-DD
;;;          t (or any other non-keyword) - YYYY-MM-DD_ (default if just 'dates' is provided without a specific keyword)
;;;          nil - no prefix
;;;
;;; Returns: A string representing the date/time prefix, or an empty string.
(defun dates-ymd (dates)
  "Create a file name from the current time,
   including optionally hour, or hours minutes and seconds as part of the file name."
  (multiple-value-bind (s min h d m y doy dstflag offset)
      (decode-universal-time (get-universal-time))
    (declare (ignore doy))
    (let* ((filename
             (cond ((equal dates :hms)
                    (format nil
                            "~4,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D_"
                            y m d
                            (+ h offset (if dstflag -1 0))
                            min s))
                   ((equal dates :hour)
                    (format nil
                            "~4,'0D-~2,'0D-~2,'0D-~2,'0D_"
                            y m d h))
                   ((equal dates :ym)
                    (format nil
                            "~4,'0D-~2,'0D_"
                            y m))
                   ((equal dates :ymd)
                    (format nil
                            "~4,'0D-~2,'0D-~2,'0D_" ; Changed to include underscore for consistent prefixing
                            y m d))
                   (dates (format nil ; This catches a non-nil, non-keyword argument, effectively the old 'dates' behavior
                                  "~4,'0D-~2,'0D-~2,'0D_"
                                  y m d))
                   (t (format nil ""))))) ; If dates is nil, return empty string
      filename)))

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

;;; Macro: WITH-OPEN-LOG-FILES
;;; Purpose: Opens multiple log files, binds them to specified variables,
;;;          executes a body of code, and ensures all log files are closed
;;;          even if errors occur, using `unwind-protect`.
;;; Each file is opened in append mode, creating it if it doesn't exist.
;;;
;;; Usage: (with-open-log-files (((stream-var-1 "path/to/file1.log" :ymd)
;;;                                (stream-var-2 "path/to/file2.log"))
;;;          body-forms...)
;;;   - log-streams: A list of lists. Each inner list must be of the form
;;;     `(variable-name "file-path-string" &optional date-prefix-keyword)`.
;;;     `variable-name` will be a symbol (e.g., `my-log-stream`) that gets
;;;     bound to the opened stream object within the scope of the macro's body.
;;;     `file-path-string` is a string representing the path to the log file.
;;;     `date-prefix-keyword` is an optional keyword (e.g., `:ymd`, `:hms`)
;;;     to prepend a date/time string to the filename. If not provided, no prefix.
;;;   - body: One or more Common Lisp forms to be executed within the context
;;;     where the log streams are open and bound.
(defmacro with-open-log-files (log-streams &body body)
  "Opens multiple log files, binds them to specified variables,
   executes a body of code, and ensures the log files are closed
   using unwind-protect. Each log file is opened in append mode.
   Optionally prefixes filenames with a date/time string."
  (let ((bindings '())      ; A list to accumulate the `let` binding forms
        (close-forms '()))  ; A list to accumulate the `close` forms for `unwind-protect`

    ;; Iterate through each stream specification provided by the user
    (dolist (stream-spec log-streams)
      ;; Destructure each specification into the variable name, file path, and optional date prefix
      (destructuring-bind (var-name file-path &optional date-prefix) stream-spec
        (let ((final-file-path (if date-prefix
                                   `(concatenate 'string (dates-ymd ,date-prefix) ,file-path)
                                   file-path)))
          ;; Add a binding form to the `bindings` list.
          (push `(,var-name (open ,final-file-path :direction :output :if-exists :append :if-does-not-exist :create))
                bindings)
          ;; Add a close form to the `close-forms` list.
          (push `(when (and (boundp ',var-name) (streamp (symbol-value ',var-name)))
                   (close (symbol-value ',var-name)))
                close-forms))))

    ;; Reverse the lists. `push` adds elements to the front, so `nreverse`
    ;; restores the original order of declarations and ensures close forms
    ;; are processed in a predictable order.
    (setf bindings (nreverse bindings))
    (setf close-forms (nreverse close-forms))

    ;; Construct the final macro expansion.
    ;; `let` establishes the lexical bindings for the stream variables.
    ;; `unwind-protect` guarantees that the `close-forms` are executed
    ;; regardless of how the `body` exits (normally or via non-local exit/error).
    `(let ,bindings
       (unwind-protect
            ;; The main body of code that uses the opened log streams
            (progn ,@body)
         ;; The cleanup forms, executed when the `unwind-protect` block is exited
         ,@close-forms))))
