;;; File: xlg.lisp
;;; Description: Contains the core logic for the XLog logging library,
;;; including the WITH-OPEN-LOG-FILES and XLG macros.
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings
(in-package #:xlg)

;; Calculate the offset from 1900-01-01 to 1970-01-01 (Unix epoch)
;; This is needed for SBCL's SB-EXT:GET-TIME-OF-DAY which returns seconds since Unix epoch,
;; while DECODE-UNIVERSAL-TIME expects seconds since 1900.
(defconstant +epoch-offset+ (encode-universal-time 0 0 0 1 1 1970 0)) ; Renamed to follow constant naming convention

;;; Function: DATES-YMD
;;; Purpose: Creates a file name prefix based on the current date and time.
;;; This function is used by WITH-OPEN-LOG-FILES for filename prefixing.
;;;
;;; Arguments:
;;;   dates: A keyword specifying the date/time format.
;;;          :hms -YYYY-MM-DD-HH-MM-SS_
;;;          :hour -YYYY-MM-DD-HH_
;;;          :ym -YYYY-MM_
;;;          :ymd -YYYY-MM-DD_
;;;          t (or any other non-keyword) -YYYY-MM-DD_ (default if just 'dates' is provided without a specific keyword)
;;;          nil - no prefix
;;;
;;; Returns: A string representing the date/time prefix, or an empty string.
(defun dates-ymd (dates)
  "Create a file name prefix from the current time,
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
                            "~4,'0D-~2,'0D-~2,'0D_" ; Includes underscore for consistent prefixing
                            y m d))
                   (dates (format nil ; Catches a non-nil, non-keyword argument, effectively the old 'dates' behavior
                                  "~4,'0D-~2,'0D-~2,'0D_"
                                  y m d))
                   (t (format nil ""))))) ; If dates is nil, return empty string
      filename)))

;;; Function: FORMATTED-CURRENT-TIME-MICRO
;;; Purpose: Produces a formatted timestamp string with microseconds.
;;; This function is used by XLG for line prefixing.
;;;
;;; Arguments:
;;;   str: An optional string to append after the timestamp.
;;;
;;; Returns: A formatted timestamp string.
(defun formatted-current-time-micro (str)
  "Produce a formatted timestamp from the current time, including microseconds."
  (multiple-value-bind (seconds microsec)
      (sb-ext:get-time-of-day)
    (multiple-value-bind (s min h d m y)
        (decode-universal-time (+ +epoch-offset+ seconds)) ; Using +epoch-offset+
      (format nil "~4,'0D-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~6,'0d ~A"
              y m d h min s microsec str))))

;;; Macro: XLG
;;; Purpose: Writes a formatted log entry to a specified log stream,
;;;          optionally prefixed with a date/time string including microseconds.
;;; It checks if the stream variable is bound and is an actual stream before writing.
;;;
;;; Usage: (xlg stream-variable format-string &rest format-and-keyword-args)
;;;   - stream-variable: The actual variable (e.g., `app-log`, `error-log`) that has been
;;;     bound to an output stream by the `WITH-OPEN-LOG-FILES` macro.
;;;   - format-string: A standard Common Lisp format control string (e.g., "~a ~s").
;;;   - format-and-keyword-args: Remaining arguments, which may include format arguments
;;;     and the keyword argument `:line-prefix` followed by its value.
;;;
;;; Returns: (No explicit return value, writes to stream)
(defmacro xlg (stream-variable format-string &rest all-args)
  (let ((line-prefix-g (gensym "LINE-PREFIX"))
        (format-args-g (gensym "FORMAT-ARGS")))
    ;; Manually parse the arguments to separate format arguments from :line-prefix
    `(let (,line-prefix-g
           (,format-args-g nil))
       (let ((temp-args ',all-args))
         (loop while temp-args do
           (if (and (consp temp-args) (eq (car temp-args) :line-prefix))
               (progn
                 (setf ,line-prefix-g (cadr temp-args))
                 (setf temp-args (cddr temp-args))) ; Skip key and value
               (progn
                 (push (car temp-args) ,format-args-g)
                 (setf temp-args (cdr temp-args)))))
         (setf ,format-args-g (nreverse ,format-args-g))) ; Reverse to maintain original order

       (when (streamp ,stream-variable)
         (let* ((prefix-string (if ,line-prefix-g
                                   (formatted-current-time-micro ,line-prefix-g)
                                   ""))
                (final-format-string (concatenate 'string prefix-string ,format-string))
                (stream ,stream-variable))
           (apply #'format stream final-format-string ,format-args-g) ; Corrected: using APPLY
           (terpri stream)
           (finish-output stream))))))

;;; Macro: WITH-OPEN-LOG-FILES
;;; Purpose: Opens multiple log files, binds them to specified variables (dynamically),
;;;          executes a body of code, and ensures all log files are closed
;;;          even if errors occur, using `unwind-protect`.
;;; Each file is opened in append mode, creating it if it doesn't exist.
;;;
;;; Usage: (with-open-log-files (((stream-var-1 "path/to/file1.log" &optional date-prefix-keyword if-exists-option)
;;;                                (stream-var-2 "path/to/file2.log"))
;;;          body-forms...)
;;;   - log-streams: A list of lists. Each inner list must be of the form
;;;     `(variable-name "file-path-string" &optional date-prefix-keyword if-exists-option)`.
;;;     `variable-name` will be a symbol (e.g., `my-log-stream`) that gets
;;;     dynamically bound to the opened stream object within the scope of the macro's body.
;;;     `file-path-string` is a string representing the path to the log file.
;;;     `date-prefix-keyword` is an optional keyword (e.g., `:ymd`, `:hms`)
;;;     to prepend a date/time string to the filename. If not provided, no prefix.
;;;     `if-exists-option` is an optional keyword (`:append` or `:replace`).
;;;     If `:replace`, the file will be overwritten. Defaults to `:append`.
;;;   - body: One or more Common Lisp forms to be executed within the context
;;;     where the log streams are open and dynamically bound.
(defmacro with-open-log-files (log-streams &body body)
  "Opens multiple log files, binds them to specified variables,
   executes a body of code, and ensures the log files are closed
   using unwind-protect. Each log file is opened in append mode.
   Optionally prefixes filenames with a date/time string.
   Provides an option to append to or replace an existing file.
   The stream variables are bound dynamically, making them accessible
   to functions called within the body's dynamic extent."
  (let ((bindings '())
        (close-forms '())
        (special-declarations '())) ; List to collect variables to declare special

    ;; Process each stream specification
    (dolist (stream-spec log-streams)
      (destructuring-bind (var-name file-path &optional date-prefix if-exists-option) stream-spec
        (let* ((final-file-path (if date-prefix
                                    `(concatenate 'string (dates-ymd ,date-prefix) ,file-path)
                                    file-path))
               (open-if-exists (cond ((eq if-exists-option :replace) :supersede)
                                     (t :append)))) ; Default to :append
          ;; Add the binding for the stream variable
          (push `(,var-name (open ,final-file-path
                                  :direction :output
                                  :if-exists ,open-if-exists
                                  :if-does-not-exist :create))
                bindings)
          ;; Add the close form for unwind-protect
          (push `(when (and (boundp ',var-name) (streamp (symbol-value ',var-name)))
                   (close (symbol-value ',var-name)))
                close-forms)
          ;; Add the variable name to the list for special declaration
          (push var-name special-declarations))))

    ;; Reverse lists to maintain original order
    (setf bindings (nreverse bindings))
    (setf close-forms (nreverse close-forms))
    (setf special-declarations (nreverse special-declarations))

    ;; Construct the final macro expansion
    `(let ,bindings
       ;; Declare all bound variables as special
       (declare (special ,@special-declarations))
       (unwind-protect
            ;; The main body of code that uses the opened log streams
            (progn ,@body)
         ;; The cleanup forms, executed when the `unwind-protect` block is exited
         ,@close-forms))))
