;;; File: xlg-lib.lisp
;;; Description: Contains the core logic for the XLog logging library,
;;; including the WITH-OPEN-LOG-FILES, XLG, and XLGT macros, and stream flushing utilities.
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings
(in-package #:xlg-lib) ; Corrected package name to :xlg-lib

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
      (:ymd (format nil "~4,'0D-~2,'0D-~2,'0D_" ; Includes underscore for consistent prefixing
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
           *log-streams*))

;;; Macro: XLG
;;; Purpose: Writes a formatted log entry to a specified log stream,
;;;          always prefixed with a date/time string including microseconds.
;;;          Returns the formatted string.
;;; It looks up the stream using a keyword from the global *LOG-STREAMS* hash table.
;;;
;;; Usage: (xlg log-keyword format-string &rest all-args)
;;;   - log-keyword: A keyword (e.g., `:APP-LOG`, `:ERROR-LOG`) that identifies
;;;     an open log stream in the *LOG-STREAMS* hash table.
;;;   - format-string: A standard Common Lisp format control string (e.g., "~a ~s").
;;;   - all-args: Remaining arguments, which may include the keyword argument
;;;     `:line-prefix` followed by its value, and positional format arguments.
;;;
;;; Returns: The formatted log string (including timestamp and line-prefix).
(defmacro xlg (log-keyword format-string &rest all-args)
  (let ((line-prefix-g (gensym "LINE-PREFIX"))
        (format-args-g (gensym "FORMAT-ARGS")))
    `(let (,line-prefix-g
           (,format-args-g nil))
       ;; Manually parse the arguments to separate format arguments from keywords
       (let ((temp-args (list ,@all-args))) ; Capture runtime values of all-args
         (loop while temp-args do
           (if (and (consp temp-args) (eq (car temp-args) :line-prefix))
               (progn
                 (setf ,line-prefix-g (cadr temp-args))
                 (setf temp-args (cddr temp-args))) ; Skip key and value
               (progn
                 (push (car temp-args) ,format-args-g)
                 (setf temp-args (cdr temp-args)))))
         (setf ,format-args-g (nreverse ,format-args-g))) ; Reverse to maintain original order

       (let* ((stream (gethash ,log-keyword *log-streams*))
              (timestamp-prefix (xlg-lib::formatted-current-time-micro (or ,line-prefix-g ""))) ; Use the parsed line-prefix
              (formatted-message (apply #'format nil (concatenate 'string timestamp-prefix ,format-string) ,format-args-g))) ; Use parsed format-args
         (when (streamp stream)
           (write-string formatted-message stream)
           (terpri stream)
           (finish-output stream))
         formatted-message)))) ; Return the formatted string

;;; Macro: XLGT
;;; Purpose: Writes a formatted log entry to a specified log stream AND to *standard-output*,
;;;          always prefixed with a date/time string including microseconds.
;;;          Returns the formatted string.
;;; It calls XLG to perform the logging to the file and get the formatted string.
;;;
;;; Usage: (xlgt log-keyword format-string &rest all-args)
;;;   - log-keyword: A keyword (e.g., `:APP-LOG`, `:ERROR-LOG`) that identifies
;;;     an open log stream in the *LOG-STREAMS* hash table.
;;;   - format-string: A standard Common Lisp format control string (e.g., "~a ~s").
;;;   - all-args: Remaining arguments, which may include the keyword argument
;;;     `:line-prefix` followed by its value, and positional format arguments.
;;;
;;; Returns: The formatted log string (including timestamp and line-prefix).
(defmacro xlgt (log-keyword format-string &rest all-args)
  `(let* ((formatted-string (xlg-lib:xlg ,log-keyword ,format-string ,@all-args))) ; Pass all-args to xlg
     ;; Always log to *standard-output*
     (write-string formatted-string *standard-output*)
     (terpri *standard-output*)
     (finish-output *standard-output*)
     formatted-string)) ; Return the formatted string


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
   using unwind-protect. Optionally prefixes filenames with a date/time string."
  (let ((let-bindings '()) ; List of (new-stream-var nil) for the top-level let
        (progn-body-forms '()) ; Forms to execute in the main body (checks, setf gethash)
        (unwind-protect-cleanup-forms '())) ; Forms for cleanup

    (dolist (stream-spec log-streams)
      (destructuring-bind (keyword-name file-path &optional date-prefix if-exists-option) stream-spec
        (unless (keywordp keyword-name)
          (error "WITH-OPEN-LOG-FILES: Stream identifier must be a keyword, but got ~S" keyword-name))

        (let ((new-stream-var (gensym (string-upcase (symbol-name keyword-name))))
              (original-value-var (gensym "ORIGINAL-VALUE"))) ; Gensym for original hash table value

          ;; Add lexical bindings
          (push `(,new-stream-var nil) let-bindings)
          (push `(,original-value-var (gethash ,keyword-name *log-streams*)) let-bindings) ; Store original value

          ;; --- Setup forms for the main progn body ---
          ;; Combine the check, open, and setf into a single progn for this stream
          (push `(progn
                   ;; 1. Runtime check for existing keyword usage - MUST be first
                   (when (gethash ,keyword-name *log-streams*)
                     (error "Log keyword ~S is already in use by an enclosing WITH-OPEN-LOG-FILES block.
                             Please use a unique keyword for nested log streams or ensure no overlap."
                            ,keyword-name))

                   ;; 2. Evaluate date prefix string at runtime and then open the file
                   (let* ((actual-date-prefix-string (if ,date-prefix (xlg-lib::dates-ymd ,date-prefix) "")) ; Evaluate at runtime
                          (open-if-exists (cond ((eq ,if-exists-option :replace) :supersede) (t :append))))
                     (setf ,new-stream-var (open (concatenate 'string actual-date-prefix-string ,file-path)
                                                :direction :output
                                                :if-exists open-if-exists
                                                :if-does-not-exist :create)))

                   ;; 3. Put the new stream into the global hash table
                   (setf (gethash ,keyword-name *log-streams*) ,new-stream-var))
                progn-body-forms)

          ;; --- Cleanup forms for unwind-protect ---
          ;; 1. Close the stream opened by this instance (if it was successfully opened)
          (push `(when (and ,new-stream-var (streamp ,new-stream-var))
                   (close ,new-stream-var))
                unwind-protect-cleanup-forms)

          ;; 2. Restore the original hash table value for this keyword
          (push `(setf (gethash ,keyword-name *log-streams*) ,original-value-var)
                unwind-protect-cleanup-forms))))

    ;; Reverse lists to maintain original order
    (setf let-bindings (nreverse let-bindings))
    (setf progn-body-forms (nreverse progn-body-forms))
    (setf unwind-protect-cleanup-forms (nreverse unwind-protect-cleanup-forms))

    ;; Construct the final macro expansion
    `(let ,let-bindings
       (unwind-protect
            (progn
              ;; Execute all open forms
              ,@progn-body-forms
              ;; The main body of code that uses the opened log streams
              ,@body)
         ;; The cleanup forms, executed when the `unwind-protect` block is exited
         (progn ; Wrap cleanup forms in a progn to ensure sequential execution
           ,@unwind-protect-cleanup-forms)))))
