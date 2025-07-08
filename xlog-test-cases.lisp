;;; File: xlog-test-cases.lisp
;;; Description: Test cases for the XLog logging library.
;;; Demonstrates logging to one, two, and three streams, including date prefixes.

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings

;; Ensure the xlg package is available
(in-package #:xlg)

;; --- Test Case 1: Logging to a single stream with YMD date prefix ---
(format t "~%--- Running Test Case 1: Single Stream Logging with YMD Prefix ---~%")
(with-open-log-files ((single-log "single-stream.log" :ymd)) ; Added :ymd prefix option
  (xlg single-log "This is the first message to a single log stream with date prefix.")
  (xlg single-log "Another message for the single log stream: ~a" "Hello, Date!")
  (format t "Messages written to date-prefixed single-stream.log~%"))

;; --- Test Case 2: Logging to two streams, one with YMD prefix, one without ---
(format t "~%--- Running Test Case 2: Two Stream Logging (One with Prefix) ---~%")
(with-open-log-files ((app-log "two-streams-app.log" :ymd) ; Added :ymd prefix option
                      (err-log "two-streams-error.log"))    ; No prefix
  (xlg app-log "Application event: User login successful for ~a (date-prefixed)" "Bill")
  (xlg err-log "Error encountered: Failed to connect to database (no prefix).")
  (xlg app-log "Application event: Data processed successfully (date-prefixed).")
  (format t "Messages written to date-prefixed two-streams-app.log and two-streams-error.log~%"))

;; --- Test Case 3: Logging to three streams, all with YMD prefix ---
(format t "~%--- Running Test Case 3: Three Stream Logging with YMD Prefix ---~%")
(with-open-log-files ((main-log "three-streams-main.log" :ymd)   ; :ymd prefix
                      (debug-log "three-streams-debug.log" :ymd) ; :ymd prefix
                      (audit-log "three-streams-audit.log" :ymd)) ; :ymd prefix
  (xlg main-log "System initialization started (main log, date-prefixed).")
  (xlg debug-log "Debug: Configuration loaded from ~a (debug log, date-prefixed)" "/etc/config.ini")
  (xlg audit-log "Audit: Access granted to IP ~a at ~a (audit log, date-prefixed)" "192.168.1.100" (get-universal-time))
  (xlg main-log "Processing user request: ~a (main log, date-prefixed)" "fetch_data")
  (xlg debug-log "Debug: Query executed in ~a ms (debug log, date-prefixed)" 150)
  (xlg audit-log "Audit: Data read by user ~a (audit log, date-prefixed)" "admin")
  (xlg main-log "System shutdown initiated (main log, date-prefixed).")
  (format t "Messages written to date-prefixed three-streams-main.log, debug.log, and audit.log~%"))

(format t "~%--- All test cases completed. Check the generated log files. ---~%")
