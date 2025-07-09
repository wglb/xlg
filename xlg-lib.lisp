;;; File: xlg-lib.lisp
;;; Description: Contains the core logic for the XLog logging library,
;;; including the WITH-OPEN-LOG-FILES and XLG macros, and stream flushing utilities.
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
;;;          optionally prefixed with a date/time string including microseconds.
;;; It looks up the stream using a keyword from the global *LOG-STREAMS* hash table.
;;;
;;; Usage: (xlg log-keyword format-string &rest format-and-keyword-args)
;;;   - log-keyword: A keyword (e.g., `:APP-LOG`, `:ERROR-LOG`) that identifies
;;;     an open log stream in the *LOG-STREAMS* hash table.
;;;   - format-string: A standard Common Lisp format control string (e.g., "~a ~s").
;;;   - format-and-keyword-args: Remaining arguments, which may include format arguments
;;;     and the keyword argument `:line-prefix` followed by its value.
;;;
;;; Returns: (No explicit return value, writes to stream)
(defmacro xlg (log-keyword format-string &rest all-args)
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

       ;; Retrieve the stream from the global *LOG-STREAMS* hash table
       (let ((stream (gethash ,log-keyword *log-streams*)))
         (when (streamp stream) ; Check if a valid stream was found
           (let* ((prefix-string (if ,line-prefix-g
                                     (formatted-current-time-micro ,line-prefix-g)
                                     ""))
                  (final-format-string (concatenate 'string prefix-string ,format-string)))
             (apply #'format stream final-format-string ,format-args-g)
             (terpri stream)
             (finish-output stream)))))))

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
  (let ((open-forms '())     ; Forms to open files and store in hash table
        (cleanup-forms '())) ; Forms to close files and remove from hash table

    ;; Process each stream specification
    (dolist (stream-spec log-streams)
      (destructuring-bind (keyword-name file-path &optional date-prefix if-exists-option) stream-spec
        (unless (keywordp keyword-name)
          (error "WITH-OPEN-LOG-FILES: Stream identifier must be a keyword, but got ~S" keyword-name))

        (let* ((final-file-path (if date-prefix
                                    `(concatenate 'string (dates-ymd ,date-prefix) ,file-path)
                                    file-path))
               (open-if-exists (cond ((eq if-exists-option :replace) :supersede)
                                     (t :append)))) ; Default to :append

          ;; Form to open the file and store in *LOG-STREAMS*
          (push `(setf (gethash ,keyword-name *log-streams*)
                       (open ,final-file-path
                             :direction :output
                             :if-exists ,open-if-exists
                             :if-does-not-exist :create))
                open-forms)

          ;; Form to close the file and remove from *LOG-STREAMS*
          (push `(let ((stream (gethash ,keyword-name *log-streams*)))
                   (when (and stream (streamp stream))
                     (close stream)
                     (remhash ,keyword-name *log-streams*)))
                cleanup-forms))))

    ;; Reverse lists to maintain original order
    (setf open-forms (nreverse open-forms))
    (setf cleanup-forms (nreverse cleanup-forms))

    ;; Construct the final macro expansion
    `(unwind-protect
          (progn
            ;; Execute all open forms
            ,@open-forms
            ;; The main body of code that uses the opened log streams
            ,@body)
       ;; The cleanup forms, executed when the `unwind-protect` block is exited
       ,@cleanup-forms)))
