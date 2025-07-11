;;; File: tests/xlg-lib-tests-suite.lisp
;;; Description: Contains the FiveAM test suite and individual test cases for the XLog library.

;; to run the tests, do
; (asdf:load-system :xlg-lib-tests)
; (asdf:test-system :xlg-lib-tests)


(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings

;; Ensure the xlg-lib-tests package is available
(in-package #:xlg-lib-tests) ; Corrected to xlg-lib-tests

;; Define the main test suite for XLog
(fiveam:def-suite xlg-lib-tests-suite
  :description "Main test suite for the XLog logging library.")

;; Set this suite as the current one for subsequent test definitions
(fiveam:in-suite xlg-lib-tests-suite)

;; --- Test Case 1: Logging to a single stream with YMD date prefix for filename and microsecond line prefix ---
(fiveam:test single-stream-logging-with-prefixes
  (format t "~%--- Running Test Case 1: Single Stream Logging with YMD Filename and Microsecond Line Prefixes ---~%")
  ;; Ensure date prefix is calculated once for consistency
  (let* ((current-date-prefix (xlg-lib::dates-ymd :ymd))
         (log-file (concatenate 'string current-date-prefix "single-stream.log")))
    (when (probe-file log-file) (delete-file log-file)) ; Clean up previous run
    ;; Pass the pre-calculated date prefix string to with-open-log-files
    (xlg-lib:with-open-log-files ((:single-log "single-stream.log" current-date-prefix))
      ;; Test XLG with :line-prefix and EXPLICIT timestamp (T)
      (let ((returned-string (xlg-lib:xlg :single-log "Message with custom prefix." :line-prefix "[TEST] " :timestamp t)))
        (fiveam:is-true (search "[TEST] Message with custom prefix." returned-string) "XLG should return string with line-prefix.")
        (fiveam:is-true (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLG should include timestamp when :timestamp is T."))

      ;; Test XLG without :line-prefix and NO timestamp (default NIL)
      (let ((returned-string (xlg-lib:xlg :single-log "Another message: ~a" "Value" :timestamp nil))) ; Explicitly nil for timestamp
        (fiveam:is-true (search "Another message: Value" returned-string) "XLG should return string without line-prefix.")
        (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLG should NOT include timestamp by default."))

      ;; Test XLG with :timestamp NIL (explicitly no timestamp)
      (let ((returned-string (xlg-lib:xlg :single-log "Message without timestamp." :timestamp nil)))
        (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLG should NOT include timestamp when :timestamp is NIL.")
        (fiveam:is-true (search "Message without timestamp." returned-string) "XLG should return message content."))

      ;; Test XLG with :timestamp NIL and :line-prefix
      (let ((returned-string (xlg-lib:xlg :single-log "Message with only line-prefix." :line-prefix "[NO-TS] " :timestamp nil)))
        (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLG should NOT include timestamp.")
        (fiveam:is-true (search "[NO-TS] Message with only line-prefix." returned-string) "XLG should include only line-prefix."))
      )
    (fiveam:is-true (probe-file log-file) "Log file 'single-stream.log' should be created.")
    (let ((content (uiop:read-file-string log-file)))
      (fiveam:is-true (search "Message with custom prefix." content) "First message should be in log file.")
      (fiveam:is-true (search "Another message: Value" content) "Second message should be in log file.")
      (fiveam:is-true (search "Message without timestamp." content) "Third message (no timestamp) should be in log file.")
      (fiveam:is-true (search "[NO-TS] Message with only line-prefix." content) "Fourth message (only line-prefix) should be in log file.")
      ;; Assert that lines without timestamp explicitly do NOT have it
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq content (search "Message without timestamp." content))) "Timestamp should not be present for :timestamp nil line.")
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq content (search "Message with only line-prefix." content))) "Timestamp should not be present for :timestamp nil line with prefix."))
    (format t "Messages written to date-prefixed single-stream.log~%")))

;; --- Test Case 2: Logging to two streams, one with YMD prefix, one without, and varied line prefixes ---
(fiveam:test two-stream-logging-varied-prefixes
  (format t "~%--- Running Test Case 2: Two Stream Logging (One Filename Prefix, Varied Line Prefixes) ---~%")
  ;; Ensure date prefix is calculated once for consistency
  (let* ((current-date-prefix (xlg-lib::dates-ymd :ymd))
         (app-log-file (concatenate 'string current-date-prefix "two-streams-app.log"))
         (err-log-file "two-streams-error.log")) ; This one has no date prefix
    (when (probe-file app-log-file) (delete-file app-log-file))
    (when (probe-file err-log-file) (delete-file err-log-file))
    ;; Pass the pre-calculated date prefix string for :app-log
    (xlg-lib:with-open-log-files ((:app-log "two-streams-app.log" current-date-prefix)
                                  (:err-log "two-streams-error.log"))
      (xlg-lib:xlg :app-log "Application event: User login successful for ~a" "Bill" :line-prefix "[APP] " :timestamp t) ; Explicit timestamp
      (xlg-lib:xlg :err-log "Error encountered: Failed to connect to database." :timestamp nil) ; No timestamp (default)
      (xlg-lib:xlg :app-log "Application event: Data processed successfully." :timestamp nil) ; No timestamp
      (xlg-lib:xlg :err-log "Another error message." :line-prefix "[WARN] " :timestamp nil)) ; No timestamp, with prefix
    (fiveam:is-true (probe-file app-log-file) "App log file should be created.")
    (fiveam:is-true (probe-file err-log-file) "Error log file should be created.")
    (let ((app-content (uiop:read-file-string app-log-file))
          (err-content (uiop:read-file-string err-log-file)))
      (fiveam:is-true (search "User login successful" app-content) "App log should have user login message.")
      (fiveam:is-true (search "Failed to connect to database" err-content) "Error log should have database error message.")
      (fiveam:is-true (search "Application event: Data processed successfully." app-content) "App log should have no-timestamp message.")
      (fiveam:is-true (search "[WARN]" err-content) "Error log should have [WARN] prefix.")
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq err-content (search "Failed to connect to database" err-content))) "Error log should NOT have timestamp for default nil.")
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq app-content (search "Data processed successfully" app-content))) "App log should NOT have timestamp for explicit nil."))
    (format t "Messages written to date-prefixed two-streams-app.log and two-streams-error.log~%")))

;; --- Test Case 3: Logging to three streams, all with YMD prefix for filename and varied line prefixes ---
(fiveam:test three-stream-logging-with-prefixes
  (format t "~%--- Running Test Case 3: Three Stream Logging with YMD Filename and Varied Line Prefixes ---~%")
  ;; Ensure date prefix is calculated once for consistency
  (let* ((current-date-prefix (xlg-lib::dates-ymd :ymd))
         (main-log-file (concatenate 'string current-date-prefix "three-streams-main.log"))
         (debug-log-file (concatenate 'string current-date-prefix "three-streams-debug.log"))
         (audit-log-file (concatenate 'string current-date-prefix "three-streams-audit.log")))
    (when (probe-file main-log-file) (delete-file main-log-file))
    (when (probe-file debug-log-file) (delete-file debug-log-file))
    (when (probe-file audit-log-file) (delete-file audit-log-file))
    ;; Pass the pre-calculated date prefix string to with-open-log-files
    (xlg-lib:with-open-log-files ((:main-log "three-streams-main.log" current-date-prefix)
                                  (:debug-log "three-streams-debug.log" current-date-prefix)
                                  (:audit-log "three-streams-audit.log" current-date-prefix))
      (xlg-lib:xlg :main-log "System initialization started." :line-prefix "[MAIN] " :timestamp t) ; Explicit timestamp
      (xlg-lib:xlg :debug-log "Debug: Configuration loaded from ~a" "/etc/config.ini" :line-prefix "[SEC-LOG] " :timestamp t) ; Explicit timestamp
      (xlg-lib:xlg :audit-log "Audit: Access granted to IP ~a at ~a" "192.168.1.100" (get-universal-time) :line-prefix "[AUDIT] " :timestamp t) ; Explicit timestamp
      (xlg-lib:xlg :main-log "Processing user request: ~a" "fetch_data" :timestamp nil) ; No timestamp
      (xlg-lib:xlg :debug-log "Debug: Query executed in ~a ms" 150 :timestamp nil) ; No timestamp (explicitly)
      (xlg-lib:xlg :audit-log "Audit: Data read by user ~a" "admin" :timestamp nil) ; No timestamp (explicitly)
      (xlg-lib:xlg :main-log "System shutdown initiated." :timestamp t)) ; Explicit timestamp
    (fiveam:is-true (probe-file main-log-file) "Main log file should be created.")
    (fiveam:is-true (probe-file debug-log-file) "Debug log file should be created.")
    (fiveam:is-true (probe-file audit-log-file) "Audit log file should be created.")
    (let ((main-content (uiop:read-file-string main-log-file))
          (debug-content (uiop:read-file-string debug-log-file))
          (audit-content (uiop:read-file-string audit-log-file)))
      (fiveam:is-true (search "System initialization started" main-content) "Main log should have init message.")
      (fiveam:is-true (search "Configuration loaded" debug-content) "Debug log should have config message.")
      (fiveam:is-true (search "Access granted" audit-content) "Audit log should have access message.")
      (fiveam:is-true (search "Processing user request: fetch_data" main-content) "Main log should have no-timestamp message.")
      (fiveam:is-true (search "Debug: Query executed in 150 ms" debug-content) "Debug log should have no-timestamp message.")
      (fiveam:is-true (search "Audit: Data read by user admin" audit-content) "Audit log should have no-timestamp message.")
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq main-content (search "Processing user request" main-content))) "Main log should NOT have timestamp for explicit nil.")
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq debug-content (search "Debug: Query executed" debug-content))) "Debug log should NOT have timestamp for explicit nil.")
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq audit-content (search "Audit: Data read" audit-content))) "Audit log should NOT have timestamp for explicit nil."))
    (format t "Messages written to date-prefixed three-streams-main.log, debug.log, and audit.log~%")))

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
    (xlg-lib:with-open-log-files ((:append-log "append-test.log"))
      (xlg-lib:xlg :append-log "Initial content for append test." :timestamp t)) ; Explicit timestamp
    (fiveam:is-true (probe-file append-file) "Append test file should be created initially.")
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string append-file))) "Append test file should have one line initially.")

    ;; Now, append more content to it
    (format t "Appending more content to 'append-test.log'.~%")
    (xlg-lib:with-open-log-files ((:append-log "append-test.log" nil :append)) ; Explicitly :append (default)
      (xlg-lib:xlg :append-log "Appended content for append test." :timestamp nil)) ; No timestamp (explicitly)
    (fiveam:is (= 2 (count #\Newline (uiop:read-file-string append-file))) "Append test file should have two lines after appending.")
    (fiveam:is-true (search "Initial content" (uiop:read-file-string append-file)) "Initial content should still be in append file.")
    (fiveam:is-true (search "Appended content" (uiop:read-file-string append-file)) "Appended content should be in append file.")
    (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq (uiop:read-file-string append-file) (search "Appended content" (uiop:read-file-string append-file)))) "Appended content should NOT have timestamp.")


    ;; Write some content to a file that will be replaced
    (format t "Writing initial content to 'replace-test.log' (will be replaced later).~%")
    (xlg-lib:with-open-log-files ((:replace-log "replace-test.log"))
      (xlg-lib:xlg :replace-log "Initial content for replace test - this should be overwritten." :timestamp t)) ; Explicit timestamp
    (fiveam:is-true (probe-file replace-file) "Replace test file should be created initially.")
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string replace-file))) "Replace test file should have one line initially.")
    (fiveam:is-true (search "Initial content" (uiop:read-file-string replace-file)) "Initial content should be in replace file.")

    ;; Now, replace the content of that file
    (format t "Replacing content of 'replace-test.log'.~%")
    (xlg-lib:with-open-log-files ((:replace-log "replace-test.log" nil :replace)) ; Explicitly :replace
      (xlg-lib:xlg :replace-log "Replaced content for replace test - this should be the ONLY line." :timestamp nil)) ; No timestamp (explicitly)
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string replace-file))) "Replace test file should have one line after replacing.")
    (fiveam:is-false (search "Initial content" (uiop:read-file-string replace-file)) "Initial content should NOT be in replace file after replacement.")
    (fiveam:is-true (search "Replaced content" (uiop:read-file-string replace-file)) "Replaced content should be in replace file.")
    (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (uiop:read-file-string replace-file)) "Replaced content should NOT have timestamp.")

    (format t "Check 'append-test.log' (should have two lines) and 'replace-test.log' (should have one line).~%")))

;; --- Test Case 5: Demonstrating explicit flushing ---
(fiveam:test explicit-flush-demonstration
  (format t "~%--- Running Test Case 5: Explicit Flush Demonstration ---~%")
  (let ((flush-test-file "flush-test.log"))
    (when (probe-file flush-test-file) (delete-file flush-test-file))

    (xlg-lib:with-open-log-files ((:flush-log flush-test-file :replace)) ; Use :replace to start fresh
      (xlg-lib:xlg :flush-log "First message, should be written immediately by XLG." :timestamp t) ; Explicit timestamp
      (xlg-lib:xlg :flush-log "Second message, also written by XLG." :timestamp t) ; Explicit timestamp

      (format t "Writing a message without XLG, then flushing all streams manually.~%")
      ;; Write directly to the stream without XLG's auto-flush
      (let ((stream (gethash :flush-log xlg-lib::*log-streams*)))
        (when (streamp stream) ; Ensure stream is valid before writing
          (format stream "~aThis is a direct write, not auto-flushed by XLG.~%" (xlg-lib::formatted-current-time-micro ""))
          (format stream "~aAnother direct message, still buffered.~%" (xlg-lib::formatted-current-time-micro ""))))

      (xlg-lib:flush-all-log-streams) ; Call the explicit flush function
      (format t "All log streams explicitly flushed.~%")

      (xlg-lib:xlg :flush-log "Third message, written after manual flush." :timestamp nil)) ; No timestamp (explicitly)
    (fiveam:is-true (probe-file flush-test-file) "Flush test file should be created.")
    (let ((content (uiop:read-file-string flush-test-file)))
      (fiveam:is-true (search "First message" content) "First message should be in log.")
      (fiveam:is-true (search "Second message" content) "Second message should be in log.")
      (fiveam:is-true (search "This message is written directly" content) "Direct message should be in log.")
      (fiveam:is-true (search "Another direct message" content) "Another direct message should be in log.")
      (fiveam:is-true (search "Third message" content) "Third message should be in log.")
      (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) (subseq content (search "Third message" content))) "Third message should NOT have timestamp."))
    (format t "Check 'flush-test.log' to see all messages, including those explicitly flushed.~%")))


;; --- Test Case 6: Testing XLGT macro (log to file and stdout) ---
(fiveam:test xlgt-macro-logging
  (format t "~%--- Running Test Case 6: Testing XLGT macro (log to file and stdout) ---~%")
  ;; Ensure date prefix is calculated once for consistency
  (let* ((current-date-prefix (xlg-lib::dates-ymd :ymd))
         (xlgt-log-file (concatenate 'string current-date-prefix "xlgt-test.log")))
    (when (probe-file xlgt-log-file) (delete-file xlgt-log-file))

    ;; Pass the pre-calculated date prefix string to with-open-log-files
    (xlg-lib:with-open-log-files ((:xlgt-stream "xlgt-test.log" current-date-prefix))
      ;; Log without line-prefix, NO timestamp (default NIL)
      (let* ((captured-output (make-string-output-stream)) ; Create a new stream for each capture
             (*standard-output* captured-output)
             (returned-string (xlg-lib:xlgt :xlgt-stream "Hello from XLGT!" :timestamp nil))) ; Explicitly nil for timestamp
        (fiveam:is-true (search "Hello from XLGT!" returned-string) "XLGT should return formatted string.")
        (fiveam:is-true (search "Hello from XLGT!" (get-output-stream-string captured-output)) "XLGT: Message should be in stdout.")
        (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLGT should NOT include timestamp by default."))

      ;; Log with line-prefix, EXPLICIT timestamp (T)
      (let* ((captured-output (make-string-output-stream)) ; Create a new stream for each capture
             (*standard-output* captured-output)
             (returned-string (xlg-lib:xlgt :xlgt-stream "Another XLGT message." :line-prefix "[Echo] " :timestamp t)))
        (fiveam:is-true (search "[Echo] Another XLGT message." returned-string) "XLGT should return formatted string with line-prefix.")
        (fiveam:is-true (search "[Echo] Another XLGT message." (get-output-stream-string captured-output)) "XLGT: Message should be in stdout with line-prefix.")
        (fiveam:is-true (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLGT should include timestamp when :timestamp is T."))

      ;; Log with :timestamp NIL (explicitly no timestamp)
      (let* ((captured-output (make-string-output-stream)) ; Create a new stream for each capture
             (*standard-output* captured-output)
             (returned-string (xlg-lib:xlgt :xlgt-stream "XLGT message without timestamp." :timestamp nil)))
        (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLGT should NOT include timestamp when :timestamp is NIL.")
        (fiveam:is-true (search "XLGT message without timestamp." returned-string) "XLGT should return message content.")
        (fiveam:is-true (search "XLGT message without timestamp." (get-output-stream-string captured-output)) "XLGT: Message should be in stdout without timestamp."))

      ;; Log with :timestamp NIL and :line-prefix
      (let* ((captured-output (make-string-output-stream)) ; Create a new stream for each capture
             (*standard-output* captured-output)
             (returned-string (xlg-lib:xlgt :xlgt-stream "XLGT message with only line-prefix." :line-prefix "[XLGT-NO-TS] " :timestamp nil)))
        (fiveam:is-false (search (subseq (xlg-lib::formatted-current-time-micro "") 0 20) returned-string) "XLG should NOT include timestamp.")
        (fiveam:is-true (search "[XLGT-NO-TS] XLGT message with only line-prefix." returned-string) "XLGT should include only line-prefix.")
        (fiveam:is-true (search "[XLGT-NO-TS] XLGT message with only line-prefix." (get-output-stream-string captured-output)) "XLGT: Message should be in stdout with only line-prefix.")))
    (fiveam:is-true (probe-file xlgt-log-file) "XLGT log file should be created.")
    (let ((content (uiop:read-file-string xlgt-log-file)))
      (fiveam:is-true (search "Hello from XLGT!" content) "XLGT: First message should be in log file.")
      (fiveam:is-true (search "Another XLGT message." content) "XLGT: Second message should be in log file.")
      (fiveam:is-true (search "XLGT message without timestamp." content) "XLGT: Third message (no timestamp) should be in log file.")
      (fiveam:is-true (search "[XLGT-NO-TS] XLGT message with only line-prefix." content) "XLGT: Fourth message (only line-prefix) should be in log file."))
    (format t "Messages written to date-prefixed xlgt-test.log and stdout.~%")) ; End of with-open-log-files

  )


;; --- Test Case 7: Nested WITH-OPEN-LOG-FILES with keyword reuse (expecting error) ---
(fiveam:test nested-with-open-log-files-error-on-reuse
  (format t "~%--- Running Test Case 7: Nested WITH-OPEN-LOG-FILES with Keyword Reuse (Expecting Error) ---~%")
  (let ((outer-log-file "outer-nested-test.log")
        (inner-log-file "inner-nested-test.log")) ; Define inner-log-file here
    (when (probe-file outer-log-file) (delete-file outer-log-file))
    (when (probe-file inner-log-file) (delete-file inner-log-file)) ; Explicit cleanup for inner file

    (xlg-lib:with-open-log-files ((:my-shared-log outer-log-file :replace)) ; Use keyword for stream
      (xlg-lib:xlg :my-shared-log "Message from outer scope before nested call." :timestamp t) ; Explicit timestamp
      (format t "Attempting to open nested log with same keyword (:MY-SHARED-LOG)...~%")

      (handler-case
          (xlg-lib:with-open-log-files ((:my-shared-log inner-log-file nil :replace)) ; This should error
            ;; This line should NOT be reached if the error is signaled correctly
            (xlg-lib:xlg :my-shared-log "This message should never be logged by inner scope." :timestamp t) ; Explicit timestamp
            (fiveam:fail "Inner with-open-log-files did NOT signal an error as expected."))
        (error (c)
          (format t "Caught expected error: ~a~%" c)
          (fiveam:pass "Successfully caught expected error for keyword reuse.")))

      (format t "Verifying outer log still works after error.~%")
      (xlg-lib:xlg :my-shared-log "Message from outer scope after nested call attempt." :timestamp t) ; Explicit timestamp
      (xlg-lib:xlg :my-shared-log "Another message after error, no timestamp." :timestamp nil) ; No timestamp (explicitly)

      (fiveam:is-true (probe-file outer-log-file) "Outer log file should still exist.")
      (let ((content (uiop:read-file-string outer-log-file)))
        (format t "nwolfeor: file is ~s~%------------------~%" content)
        (fiveam:is-true (search "Message from outer scope before nested call." content) "Outer log: first message present.")
        (fiveam:is-true (search "Message from outer scope after nested call attempt." content) "Outer log: second message present.")
        (fiveam:is-false (search "This message should never be logged" content) "Inner message should NOT be in outer log.")
        (fiveam:is-false (probe-file inner-log-file) "Inner log file should NOT be created if error signaled early.")))))

;; Function to run all tests in the suite
(defun run-xlg-tests ()
  "Runs all tests defined in the XLG-LIB-TESTS-SUITE."
  (let ((*default-pathname-defaults* #P"")) ; Ensure tests run in current directory
    (fiveam:run! 'xlg-lib-tests-suite)))

(format t "~%--- All test cases defined. Use (asdf:test-system :xlg-lib-tests) to run them. ---~%")
