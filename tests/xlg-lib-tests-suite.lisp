;;; File: tests/xlg-lib-tests-suite.lisp
;;; Description: Contains the FiveAM test suite and individual test cases for the XLog library.

;; to run the tests, do
; (asdf:load-system :xlg-lib-tests)
; (asdf:test-system :xlg-lib-tests)


(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings

;; Ensure the xlg-lib-tests package is available
(in-package #:xlg-lib-tests) ; Corrected package name

;; Define the main test suite for XLog
(fiveam:def-suite xlg-lib-tests-suite ; Suite name corrected
  :description "Main test suite for the XLog logging library.")

;; Set this suite as the current one for subsequent test definitions
(fiveam:in-suite xlg-lib-tests-suite)

;; --- Test Case 1: Logging to a single stream with YMD date prefix for filename and microsecond line prefix ---
(fiveam:test single-stream-logging-with-prefixes
  (format t "~%--- Running Test Case 1: Single Stream Logging with YMD Filename and Microsecond Line Prefixes ---~%")
  ;; CORRECTED: Use concatenate 'string for log-file path construction
  (let ((log-file (merge-pathnames (xlg-lib::dates-ymd :ymd) "single-stream.log")))
    (when (probe-file log-file) (delete-file log-file)) ; Clean up previous run
    (xlg-lib:with-open-log-files ((:single-log "single-stream.log" :ymd)) ; Use keyword for stream
      ;; Line prefixed with microsecond timestamp, no additional string
      (xlg-lib:xlg :single-log "This is the first message to a single log stream with date prefix." :line-prefix "")
      ;; Line prefixed with microsecond timestamp and a custom string
      (xlg-lib:xlg :single-log "Another message for the single log stream: ~a" "Hello, Microseconds!" :line-prefix "[SEC-LOG] "))
    (fiveam:is-true (probe-file log-file) "Log file 'single-stream.log' should be created.")
    (let ((content (uiop:read-file-string log-file)))
      (fiveam:is-true (search "This is the first message" content) "First message should be in log.")
      (fiveam:is-true (search "Another message for the single log stream" content) "Second message should be in log.")
      (fiveam:is-true (search "[SEC-LOG]" content) "Second message should have [SEC-LOG] prefix."))
    (format t "Messages written to date-prefixed single-stream.log~%")))

;; --- Test Case 2: Logging to two streams, one with YMD prefix, one without, and varied line prefixes ---
(fiveam:test two-stream-logging-varied-prefixes
  (format t "~%--- Running Test Case 2: Two Stream Logging (One Filename Prefix, Varied Line Prefixes) ---~%")
  ;; CORRECTED: Use concatenate 'string for log-file path construction
  (let ((app-log-file (merge-pathnames (xlg-lib::dates-ymd :ymd) "two-streams-app.log"))
        (err-log-file "two-streams-error.log"))
    (when (probe-file app-log-file) (delete-file app-log-file))
    (when (probe-file err-log-file) (delete-file err-log-file))
    (xlg-lib:with-open-log-files ((:app-log "two-streams-app.log" :ymd) ; Use keyword for stream
                                  (:err-log "two-streams-error.log"))    ; Use keyword for stream
      ;; Line prefixed with microsecond timestamp and a custom string
      (xlg-lib:xlg :app-log "Application event: User login successful for ~a (date-prefixed filename and line)" "Bill" :line-prefix "[APP] ")
      ;; Line prefixed with microsecond timestamp, no additional string
      (xlg-lib:xlg :err-log "Error encountered: Failed to connect to database (no filename prefix, but line has microsecond date)." :line-prefix "[SEC-LOG] ")
      ;; No line prefix
      (xlg-lib:xlg :app-log "Application event: Data processed successfully (date-prefixed filename, no line prefix)." "date-prefixed filename, no line prefix")
      ;; No line prefix
      (xlg-lib:xlg :err-log "Another error message (no filename prefix, no line prefix)."))
    (fiveam:is-true (probe-file app-log-file) "App log file should be created.")
    (fiveam:is-true (probe-file err-log-file) "Error log file should be created.")
    (let ((app-content (uiop:read-file-string app-log-file))
          (err-content (uiop:read-file-string err-log-file)))
      (fiveam:is-true (search "[APP]" app-content) "App log should have [APP] prefix.")
      (fiveam:is-true (search "[SEC-LOG]" err-content) "Error log should have [SEC-LOG] prefix."))
    (format t "Messages written to date-prefixed two-streams-app.log and two-streams-error.log~%")))

;; --- Test Case 3: Logging to three streams, all with YMD prefix for filename and varied line prefixes ---
(fiveam:test three-stream-logging-with-prefixes
  (format t "~%--- Running Test Case 3: Three Stream Logging with YMD Filename and Varied Line Prefixes ---~%")
  (let ((main-log-file (merge-pathnames (xlg-lib::dates-ymd :ymd) "three-streams-main.log"))
        (debug-log-file (merge-pathnames (xlg-lib::dates-ymd :ymd) "three-streams-debug.log"))
        (audit-log-file (merge-pathnames (xlg-lib::dates-ymd :ymd) "three-streams-audit.log")))
    (when (probe-file main-log-file) (delete-file main-log-file))
    (when (probe-file debug-log-file) (delete-file debug-log-file))
    (when (probe-file audit-log-file) (delete-file audit-log-file))
    (xlg-lib:with-open-log-files ((:main-log "three-streams-main.log" :ymd)
                                  (:debug-log "three-streams-debug.log" :ymd)
                                  (:audit-log "three-streams-audit.log" :ymd))
      (xlg-lib:xlg :main-log "System initialization started (main log, filename & line prefixed)." :line-prefix "[MAIN] ")
      (xlg-lib:xlg :debug-log "Debug: Configuration loaded from ~a (debug log, filename & line prefixed)" "/etc/config.ini" :line-prefix "[SEC-LOG] ")
      (xlg-lib:xlg :audit-log "Audit: Access granted to IP ~a at ~a (audit log, filename & line prefixed)" "192.168.1.100" (get-universal-time) :line-prefix "[AUDIT] ")
      (xlg-lib:xlg :main-log "Processing user request: ~a (main log, filename prefixed, no line prefix)" "fetch_data")
      (xlg-lib:xlg :debug-log "Debug: Query executed in ~a ms (debug log, filename prefixed, no line prefix)" 150)
      (xlg-lib:xlg :audit-log "Audit: Data read by user ~a (audit log, filename prefixed, no line prefix)" "admin")
      (xlg-lib:xlg :main-log "System shutdown initiated (main log, filename & line prefixed)." :line-prefix "[MAIN] "))
    (fiveam:is-true (probe-file main-log-file) "Main log file should be created.")
    (fiveam:is-true (probe-file debug-log-file) "Debug log file should be created.")
    (fiveam:is-true (probe-file audit-log-file) "Audit log file should be created.")
    (let ((main-content (uiop:read-file-string main-log-file))
          (debug-content (uiop:read-file-string debug-log-file))
          (audit-content (uiop:read-file-string audit-log-file)))
      (fiveam:is-true (search "[MAIN]" main-content) "Main log should have [MAIN] prefix.")
      (fiveam:is-true (search "[SEC-LOG]" debug-content) "Debug log should have [SEC-LOG] prefix.")
      (fiveam:is-true (search "[AUDIT]" audit-content) "Audit log should have [AUDIT] prefix."))
    (format t "Messages written to date-prefixed three-streams-main.log, debug.log, and audit.log~%")))

;; --- Test Case 4: Demonstrating append vs. replace ---
(fiveam:test append-vs-replace-file-opening
  (format t "~%--- Running Test Case 4: Append vs. Replace File Opening ---~%")
  (let ((append-file "append-test.log")
        (replace-file "replace-test.log"))
    ;; Clean up previous runs
    (when (probe-file append-file) (delete-file append-file))
    (when (probe-file replace-file) (delete-file replace-file))

    ;; First, write some content to a file that will be appended to
    (format t "Writing initial content to 'append-test.log' (will be appended to later).~%")
    (xlg-lib:with-open-log-files ((:append-log "append-test.log"))
      (xlg-lib:xlg :append-log "Initial content for append test."))
    (fiveam:is-true (probe-file append-file) "Append test file should be created initially.")
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string append-file))) "Append test file should have one line initially.")

    ;; Now, append more content to it
    (format t "Appending more content to 'append-test.log'.~%")
    (xlg-lib:with-open-log-files ((:append-log "append-test.log" nil :append)) ; Explicitly :append (default)
      (xlg-lib:xlg :append-log "Appended content for append test."))
    (fiveam:is (= 2 (count #\Newline (uiop:read-file-string append-file))) "Append test file should have two lines after appending.")
    (fiveam:is-true (search "Initial content" (uiop:read-file-string append-file)) "Initial content should still be in append file.")
    (fiveam:is-true (search "Appended content" (uiop:read-file-string append-file)) "Appended content should be in append file.")

    ;; Write some content to a file that will be replaced
    (format t "Writing initial content to 'replace-test.log' (will be replaced later).~%")
    (xlg-lib:with-open-log-files ((:replace-log "replace-test.log"))
      (xlg-lib:xlg :replace-log "Initial content for replace test - this should be overwritten."))
    (fiveam:is-true (probe-file replace-file) "Replace test file should be created initially.")
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string replace-file))) "Replace test file should have one line initially.")
    (fiveam:is-true (search "Initial content" (uiop:read-file-string replace-file)) "Initial content should be in replace file.")

    ;; Now, replace the content of that file
    (format t "Replacing content of 'replace-test.log'.~%")
    (xlg-lib:with-open-log-files ((:replace-log "replace-test.log" nil :replace)) ; Explicitly :replace
      (xlg-lib:xlg :replace-log "Replaced content for replace test - this should be the ONLY line."))
    (fiveam:is (= 1 (count #\Newline (uiop:read-file-string replace-file))) "Replace test file should have one line after replacing.")
    (fiveam:is-false (search "Initial content" (uiop:read-file-string replace-file)) "Initial content should NOT be in replace file after replacement.")
    (fiveam:is-true (search "Replaced content" (uiop:read-file-string replace-file)) "Replaced content should be in replace file.")

    (format t "Check 'append-test.log' (should have two lines) and 'replace-test.log' (should have one line).~%")))

;; --- Test Case 5: Demonstrating explicit flushing ---
(fiveam:test explicit-flush-demonstration
  (format t "~%--- Running Test Case 5: Explicit Flush Demonstration ---~%")
  (let ((flush-test-file "flush-test.log"))
    (when (probe-file flush-test-file) (delete-file flush-test-file))

    (xlg-lib:with-open-log-files ((:flush-log flush-test-file :replace)) ; Use :replace to start fresh
      (xlg-lib:xlg :flush-log "First message, should be written immediately by XLG.")
      (xlg-lib:xlg :flush-log "Second message, also written by XLG.")

      (format t "Writing a message without XLG, then flushing all streams manually.~%")
      ;; Write directly to the stream without XLG's auto-flush
      (let ((stream (gethash :flush-log xlg-lib::*log-streams*)))
        (when (streamp stream)
          (format stream "This message is written directly to the stream, not via XLG.~%")
          (format stream "Another direct message, still not flushed by XLG.~%")))

      (xlg-lib:flush-all-log-streams) ; Call the explicit flush function
      (format t "All log streams explicitly flushed.~%")

      (xlg-lib:xlg :flush-log "Third message, written after manual flush.")
      )
    (fiveam:is-true (probe-file flush-test-file) "Flush test file should be created.")
    (let ((content (uiop:read-file-string flush-test-file)))
      (fiveam:is-true (search "First message" content) "First message should be in log.")
      (fiveam:is-true (search "Second message" content) "Second message should be in log.")
      (fiveam:is-true (search "This message is written directly" content) "Direct message should be in log.")
      (fiveam:is-true (search "Another direct message" content) "Another direct message should be in log.")
      (fiveam:is-true (search "Third message" content) "Third message should be in log."))
    (format t "Check 'flush-test.log' to see all messages, including those explicitly flushed.~%")))


;; --- New Test Case: Nested WITH-OPEN-LOG-FILES with keyword reuse (expecting error) ---
(fiveam:test nested-with-open-log-files-error-on-reuse
  (format t "~%--- Running New Test Case: Nested WITH-OPEN-LOG-FILES with Keyword Reuse (Expecting Error) ---~%")
  (let ((outer-log-file "outer-nested-test.log"))
    (when (probe-file outer-log-file) (delete-file outer-log-file))

    (xlg-lib:with-open-log-files ((:my-shared-log outer-log-file :replace)) ; Use keyword for stream
      (xlg-lib:xlg :my-shared-log "Message from outer scope before nested call.")
      (format t "Attempting to open nested log with same keyword (:MY-SHARED-LOG)...~%")

      (handler-case
          (xlg-lib:with-open-log-files ((:my-shared-log "inner-nested-test.log" nil :replace)) ; This should error
            ;; This line should NOT be reached if the error is signaled correctly
            (xlg-lib:xlg :my-shared-log "This message should never be logged by inner scope.")
            (fiveam:fail "Inner with-open-log-files did NOT signal an error as expected.")) ; Fail if no error
        (error (c)
          (format t "Caught expected error: ~a~%" c)
          (fiveam:pass "Successfully caught expected error for keyword reuse."))) ; Pass if error caught

      (format t "Verifying outer log still works after error.~%")
      (xlg-lib:xlg :my-shared-log "Message from outer scope after nested call attempt.") ; This should work

      (fiveam:is-true (probe-file outer-log-file) "Outer log file should still exist.")
      (let ((content (uiop:read-file-string outer-log-file)))
        (format t "nwolfeor: file is ~s~%------------------~%" content)
        (fiveam:is-true (search "Message from outer scope before nested call." content) "Outer log: first message present.")
        (fiveam:is-true (search "Message from outer scope after nested call attempt." content) "Outer log: second message present.")
        (fiveam:is-false (search "This message should never be logged" content) "Inner message should NOT be in outer log.")
        (fiveam:is-false (probe-file "inner-nested-test.log") "Inner log file should NOT be created if error signaled early.")))))

;; Function to run all tests in the suite
(defun run-xlg-tests ()
  "Runs all tests defined in the XLG-LIB-TESTS-SUITE."
  (let ((*default-pathname-defaults* #P"")) ; Ensure tests run in current directory
    (fiveam:run! 'xlg-lib-tests-suite)))

(format t "~%--- All test cases defined. Use (asdf:test-system :xlg-lib-tests) to run them. ---~%")
