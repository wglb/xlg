;;; File: xlg-lib.lisp
;;; Description: Contains the core logic for the XLog logging library,
;;; including the WITH-OPEN-LOG-FILES and XLG macros, and stream flushing utilities.
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings
(in-package #:xlg-lib) ; Package name changed to :xlg-lib

;; Calculate the offset from 1900-01-01 to 1970-01-01 (Unix epoch)
;; This is needed for SBCL's SB-EXT:GET-TIME-OF-DAY which returns seconds since Unix epoch,
;; while DECODE-UNIVERSAL-TIME expects seconds since 1900.
(defconstant +epoch-offset+ (encode-universal-time 0 0 0 1 1 1970 0)) ; Renamed to follow constant naming convention

;;; Special Variable: *LOG-STREAMS*
;;; Purpose: A global hash table to store currently open log streams,
;;;          mapped by their keyword identifiers. This allows dynamic access.
(defvar *log-streams* (make-hash-table :test 'eq)
  "A hash table mapping log stream keywords to their open stream objects.")

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
    (case dates
      (:hms (format nil "~4,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D-~2,'0D_"
                    y m d (+ h offset (if dstflag -1 0)) min s))
      (:hour (format nil "~4,'0D-~2,'0D-~2,'0D-~2,'0D_"
                     y m d h))
      (:ym (format nil "~4,'0D-~2,'0D_"
                   y m))
      (:ymd (format nil "~4,'00D-~2,'0D-~2,'0D_" ; Includes underscore for consistent prefixing
                    y m d))
      ;; If 'dates' is non-NIL but not one of the specific keywords,
      ;; assume the default YMD_ format.
      ((t) (format nil "~4,'0D-~2,'0D-~2,'0D_"
                   y m d))
      ;; If 'dates' is NIL or any other unrecognized value, return empty string
      (otherwise (format nil "")))))

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

;;; Function: FLUSH-ALL-LOG-STREAMS
;;; Purpose: Flushes the output buffers of all currently open log streams
;;;          managed by *LOG-STREAMS*.
;;; This ensures that all buffered log entries are written to their respective files.
(defun flush-all-log-streams ()
  "Flushes the output buffers of all currently open log streams managed by *LOG-STREAMS*."
  (maphash (lambda (key stream)
             (declare (ignore key)) ; Key is not used here
             (when (streamp stream)
               (finish-output stream)))
           *log-streams*)
  (finish-output)) ; Added to flush *standard-output* as well

;;; Macro: XLG
;;; Purpose: Writes a formatted log entry to a specified log stream,
;;;          optionally prefixed with a date/time string including microseconds.
;;;          Optionally echoes the output to standard output.
;;; It looks up the stream using a keyword from the global *LOG-STREAMS* hash table.
;;;
;;; Usage: (xlg log-keyword format-string &rest format-and-keyword-args &key line-prefix stdout)
;;;   - log-keyword: A keyword (e.g., `:APP-LOG`, `:ERROR-LOG`) that identifies
;;;     an open log stream in the *LOG-STREAMS* hash table.
;;;   - format-string: A standard Common Lisp format control string (e.g., "~a ~s").
;;;   - format-and-keyword-args: Remaining arguments, which may include format arguments
;;;     and the keyword arguments `:line-prefix` and `:stdout`.
;;;   - :line-prefix: An optional string to append after the microsecond timestamp
;;;     for the log entry line. If NIL, no timestamp is added.
;;;   - :stdout: If true, the formatted log message will also be printed
;;;     to *standard-output*.
;;;
;;; Returns: (No explicit return value, writes to stream)
(defmacro xlg (log-keyword format-string &rest all-args)
  (let ((line-prefix-g (gensym "LINE-PREFIX"))
        (echo-to-stdout-g (gensym "ECHO-TO-STDOUT")) ; New gensym for echo option
        (format-args-g (gensym "FORMAT-ARGS")))
    ;; Manually parse the arguments to separate format arguments and keyword arguments
    `(let (,line-prefix-g
           ,echo-to-stdout-g ; Initialize new gensym
           (,format-args-g nil))
       (let ((temp-args ',all-args))
         (loop while temp-args do
           (if (and (consp temp-args) (eq (car temp-args) :line-prefix))
               (progn
                 (setf ,line-prefix-g (cadr temp-args))
                 (setf temp-args (cddr temp-args))) ; Skip key and value
               (if (and (consp temp-args) (eq (car temp-args) :stdout)) ; Parse new keyword
                   (progn
                     (setf ,echo-to-stdout-g (cadr temp-args))
                     (setf temp-args (cddr temp-args))) ; Skip key and value
                   (progn
                     (push (car temp-args) ,format-args-g)
                     (setf temp-args (cdr temp-args))))))
         (setf ,format-args-g (nreverse ,format-args-g))) ; Reverse to maintain original order

       ;; Retrieve the stream from the global *LOG-STREAMS* hash table
       (let* ((stream (gethash ,log-keyword *log-streams*))
              (prefix-string (if ,line-prefix-g
                                 (formatted-current-time-micro ,line-prefix-g)
                                 ""))
              (final-format-string (concatenate 'string prefix-string ,format-string)))
         (when (streamp stream) ; Check if a valid stream was found
           (apply #'format stream final-format-string ,format-args-g)
           (terpri stream)
           (finish-output stream))

         ;; Echo to *standard-output* if requested
         (when ,echo-to-stdout-g
           (apply #'format *standard-output* final-format-string ,format-args-g)
           (terpri *standard-output*))))))

;;; Macro: WITH-OPEN-LOG-FILES
;;; Purpose: Opens multiple log files, stores them in the *LOG-STREAMS* hash table
;;;          under specified keywords, executes a body of code, and ensures
;;;          all log files are closed and removed from the hash table upon exit.
;;; Each file is opened in append mode, creating it if it doesn't exist.
;;;
;;; Usage: (with-open-log-files (((:stream-keyword-1 "path/to/file1.log" &optional date-prefix-keyword if-exists-option)
;;;                                (:stream-keyword-2 "path/to/file2.log"))
;;;          body-forms...)
;;;   - log-streams: A list of lists. Each inner list must be of the form
;;;     `(keyword-name "file-path-string" &optional date-prefix-keyword if-exists-option)`.
;;;     `keyword-name` will be a keyword (e.g., `:MY-LOG-STREAM`) used as a key
;;;     in the *LOG-STREAMS* hash table.
;;;     `file-path-string` is a string representing the path to the log file.
;;;     `date-prefix-keyword` is an optional keyword (e.g., `:ymd`, `:hms`)
;;;     to prepend a date/time string to the filename. If not provided, no prefix.
;;;     `if-exists-option` is an optional keyword (`:append` or `:replace`).
;;;     If `:replace`, the file will be overwritten. Defaults to `:append`.
;;;   - body: One or more Common Lisp forms to be executed within the context
;;;     where the log streams are open and accessible via *LOG-STREAMS*.
(defmacro with-open-log-files (log-streams &body body)
  "Opens multiple log files, stores them in *LOG-STREAMS* under specified keywords,
   executes a body of code, and ensures files are closed and removed from *LOG-STREAMS*
   using unwind-protect. Optionally prefixes filenames with a date/time string.
   Signals an error if a log keyword is already in use by an enclosing WITH-OPEN-LOG-FILES block."
  (let ((let-bindings '()) ; For lexical variables (new-stream-var, original-hash-value-var)
        (progn-body-forms '()) ; Forms to execute in the main body (checks, setf gethash)
        (unwind-protect-cleanup-forms '())) ; Forms for cleanup

    (dolist (stream-spec log-streams)
      (destructuring-bind (keyword-name file-path &optional date-prefix if-exists-option) stream-spec
        (unless (keywordp keyword-name)
          (error "WITH-OPEN-LOG-FILES: Stream identifier must be a keyword, but got ~S" keyword-name))

        (let ((new-stream-var (gensym (string-upcase (symbol-name keyword-name))))
              (original-hash-value-var (gensym "ORIG-HASH-VAL")))

          ;; Add lexical bindings for the new stream and its original hash table value
          (push `(,new-stream-var nil) let-bindings) ; New stream will be set later
          (push `(,original-hash-value-var (gethash ,keyword-name *log-streams*)) let-bindings)

          ;; --- Setup forms for the main progn body ---
          ;; 1. Runtime check for existing keyword usage
          (push `(when (gethash ,keyword-name *log-streams*)
                   (error "Log keyword ~S is already in use by an enclosing WITH-OPEN-LOG-FILES block.
                           Please use a unique keyword for nested log streams or ensure no overlap."
                          ,keyword-name))
                progn-body-forms)

          ;; 2. Open the new stream and assign to lexical variable
          (let* ((date-prefix-string-form (if date-prefix `(xlg-lib::dates-ymd ,date-prefix) ""))
                 (open-if-exists (cond ((eq if-exists-option :replace) :supersede) (t :append))))
            (push `(setf ,new-stream-var (open (concatenate 'string ,date-prefix-string-form ,file-path)
                                               :direction :output
                                               :if-exists ,open-if-exists
                                               :if-does-not-exist :create))
                  progn-body-forms))

          ;; 3. Put the new stream into the global hash table
          (push `(setf (gethash ,keyword-name *log-streams*) ,new-stream-var)
                progn-body-forms)

          ;; --- Cleanup forms for unwind-protect ---
          ;; 1. Close the stream opened by this instance (if it was successfully opened)
          (push `(when (and ,new-stream-var (streamp ,new-stream-var))
                   (format t "WITH-OPEN-LOG-FILES: Closing stream ~s for keyword ~s~%" ,new-stream-var ,keyword-name)
                   (close ,new-stream-var))
                unwind-protect-cleanup-forms)

          ;; 2. Restore the original hash table value for this keyword
          (push `(setf (gethash ,keyword-name *log-streams*) ,original-hash-value-var)
                unwind-protect-cleanup-forms))))

    (setf let-bindings (nreverse let-bindings))
    (setf progn-body-forms (nreverse progn-body-forms))
    (setf unwind-protect-cleanup-forms (nreverse unwind-protect-cleanup-forms))

    `(let ,let-bindings
       (unwind-protect
            (progn
              ,@progn-body-forms
              ,@body)
         (progn
           ,@unwind-protect-cleanup-forms)))))
