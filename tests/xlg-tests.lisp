;;; File: xlog-test-cases.lisp
;;; Description: Test cases for the XLog logging library.
;;; Demonstrates logging to one, two, and three streams, including date prefixes.
;;; Also includes a test case for the append/replace option, using keyword identifiers.
;;; Includes a demonstration of FLUSH-ALL-LOG-STREAMS.

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings

;; Ensure the xlg package is available
(in-package #:xlg)

;; --- Test Case 1: Logging to a single stream with YMD date prefix for filename and microsecond line prefix ---
(format t "~%--- Running Test Case 1: Single Stream Logging with YMD Filename and Microsecond Line Prefixes ---~%")
;; Note: Now passing :SINGLE-LOG as the keyword identifier
(with-open-log-files ((:single-log "single-stream.log" :ymd)) ; Filename prefixed with YMD
  ;; Line prefixed with microsecond timestamp, no additional string
  (xlg :single-log "This is the first message to a single log stream with date prefix." :line-prefix "")
  ;; Line prefixed with microsecond timestamp and a custom string
  (xlg :single-log "Another message for the single log stream: ~a" "Hello, Microseconds!" :line-prefix "[SEC-LOG] ")
  (format t "Messages written to date-prefixed single-stream.log~%"))

;; --- Test Case 2: Logging to two streams, one with YMD prefix, one without, and varied line prefixes ---
(format t "~%--- Running Test Case 2: Two Stream Logging (One Filename Prefix, Varied Line Prefixes) ---~%")
;; Note: Now passing :APP-LOG and :ERR-LOG as keyword identifiers
(with-open-log-files ((:app-log "two-streams-app.log" :ymd) ; Filename prefixed with YMD
                      (:err-log "two-streams-error.log"))    ; No filename prefix
  ;; Line prefixed with microsecond timestamp and a custom string
  (xlg :app-log "Application event: User login successful for ~a (date-prefixed filename and line)" "Bill" :line-prefix "[APP] ")
  ;; Line prefixed with microsecond timestamp, no additional string
  (xlg :err-log "Error encountered: Failed to connect to database (no filename prefix, but line has microsecond date)." :line-prefix "[SEC-LOG] ")
  ;; No line prefix
  (xlg :app-log "Application event: Data processed successfully (date-prefixed filename, no line prefix)." "date-prefixed filename, no line prefix")
  ;; No line prefix
  (xlg :err-log "Another error message (no filename prefix, no line prefix).")
  (format t "Messages written to date-prefixed two-streams-app.log and two-streams-error.log~%"))

;; --- Test Case 3: Logging to three streams, all with YMD prefix for filename and varied line prefixes ---
(format t "~%--- Running Test Case 3: Three Stream Logging with YMD Filename and Varied Line Prefixes ---~%")
;; Note: Now passing :MAIN-LOG, :DEBUG-LOG, :AUDIT-LOG as keyword identifiers
(with-open-log-files ((:main-log "three-streams-main.log" :ymd)   ; :ymd filename prefix
                      (:debug-log "three-streams-debug.log" :ymd) ; :ymd filename prefix
                      (:audit-log "three-streams-audit.log" :ymd)) ; :ymd filename prefix
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg :main-log "System initialization started (main log, filename & line prefixed)." :line-prefix "[MAIN] ")
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg :debug-log "Debug: Configuration loaded from ~a (debug log, filename & line prefixed)" "/etc/config.ini" :line-prefix "[SEC-LOG] ")
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg :audit-log "Audit: Access granted to IP ~a at ~a (audit log, filename & line prefixed)" "192.168.1.100" (get-universal-time) :line-prefix "[AUDIT] ")
  ;; No line prefix
  (xlg :main-log "Processing user request: ~a (main log, filename prefixed, no line prefix)" "fetch_data")
  ;; No line prefix
  (xlg :debug-log "Debug: Query executed in ~a ms (debug log, filename prefixed, no line prefix)" 150)
  ;; No line prefix
  (xlg :audit-log "Audit: Data read by user ~a (audit log, filename prefixed, no line prefix)" "admin")
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg :main-log "System shutdown initiated (main log, filename & line prefixed)." :line-prefix "[MAIN] ")
  (format t "Messages written to date-prefixed three-streams-main.log, debug.log, and audit.log~%"))

;; --- Test Case 4: Demonstrating append vs. replace ---
(fiveam:test append-vs-replace-file-opening
  (format t "~%--- Running Test Case 4: Append vs. Replace File Opening ---~%")
  (let ((append-file "append-test.log")
        (replace-file "replace-test.log"))
    ;; Clean up previous runs (note: these files won't have date prefixes)
    (when (probe-file append-file) (delete-file append-file))
    (when (probe-file replace-file) (delete-file replace-file))

    ;; First, write some content to a file that will be appended to
    (format t "Writing initial content to 'append-test.log' (will be appended to later).~%")
    ;; Note: Now passing :APPEND-LOG as the keyword identifier
    (with-open-log-files ((:append-log "append-test.log"))
      (xlg :append-log "Initial content for append test."))
    (fiveam:is-true (probe-file append-file) "Append test file should be created initially.")
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string append-file))) "Append test file should have one line initially.")

    ;; Now, append more content to it
    (format t "Appending more content to 'append-test.log'.~%")
    (with-open-log-files ((:append-log "append-test.log" nil :append)) ; Explicitly :append (default)
      (xlg :append-log "Appended content for append test."))
    (fiveam:is (= 2 (count #\Newline (uiop:read-file-string append-file))) "Append test file should have two lines after appending.")
    (fiveam:is-true (search "Initial content" (uiop:read-file-string append-file)) "Initial content should still be in append file.")
    (fiveam:is-true (search "Appended content" (uiop:read-file-string append-file)) "Appended content should be in append file.")

    ;; Write some content to a file that will be replaced
    (format t "Writing initial content to 'replace-test.log' (will be replaced later).~%")
    (with-open-log-files ((:replace-log "replace-test.log"))
      (xlg :replace-log "Initial content for replace test - this should be overwritten."))
    (fiveam:is-true (probe-file replace-file) "Replace test file should be created initially.")
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string replace-file))) "Replace test file should have one line initially.")
    (fiveam:is-true (search "Initial content" (uiop:read-file-string replace-file)) "Initial content should be in replace file.")

    ;; Now, replace the content of that file
    (format t "Replacing content of 'replace-test.log'.~%")
    (with-open-log-files ((:replace-log "replace-test.log" nil :replace)) ; Explicitly :replace
      (xlg :replace-log "Replaced content for replace test - this should be the ONLY line."))
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string replace-file))) "Replace test file should have one line after replacing.")
    (fiveam:is-false (search "Initial content" (uiop:read-file-string replace-file)) "Initial content should NOT be in replace file after replacement.")
    (fiveam:is-true (search "Replaced content" (uiop:read-file-string replace-file)) "Replaced content should be in replace file.")

    (format t "Check 'append-test.log' (should have two lines) and 'replace-test.log' (should have one line).~%")))

;; --- Test Case 5: Demonstrating explicit flushing ---
(fiveam:test explicit-flush-demonstration
  (format t "~%--- Running Test Case 5: Explicit Flush Demonstration ---~%")
  (let ((flush-test-file "flush-test.log"))
    (when (probe-file flush-test-file) (delete-file flush-test-file))

    (with-open-log-files ((:flush-log flush-test-file :replace)) ; Use :replace to start fresh
      (xlg :flush-log "First message, should be written immediately by XLG.")
      (xlg :flush-log "Second message, also written by XLG.")

      (format t "Writing a message without XLG, then flushing all streams manually.~%")
      ;; Write directly to the stream without XLG's auto-flush
      (let ((stream (gethash :flush-log *log-streams*)))
        (format stream "This message is written directly to the stream, not via XLG.~%")
        (format stream "Another direct message, still not flushed by XLG.~%"))

      ;; Now, explicitly flush all log streams
      (flush-all-log-streams)
      (format t "All log streams explicitly flushed.~%")

      (xlg :flush-log "Third message, written after manual flush.")
      )
    (fiveam:is-true (probe-file flush-test-file) "Flush test file should be created.")
    (let ((content (uiop:read-file-string flush-test-file)))
      (fiveam:is-true (search "First message" content) "First message should be in log.")
      (fiveam:is-true (search "Second message" content) "Second message should be in log.")
      (fiveam:is-true (search "This message is written directly" content) "Direct message should be in log.")
      (fiveam:is-true (search "Another direct message" content) "Another direct message should be in log.")
      (fiveam:is-true (search "Third message" content) "Third message should be in log."))
    (format t "Check 'flush-test.log' to see all messages, including those explicitly flushed.~%")))


;; Function to run all tests in the suite
(defun run-xlg-tests ()
  "Runs all tests defined in the XLG-TESTS-SUITE."
  (let ((*default-pathname-defaults* #P"")) ; Ensure tests run in current directory
    (fiveam:run! 'xlg-tests-suite)))

(format t "~%--- All test cases defined. Use (asdf:test-system :xlg-tests) to run them. ---~%")
