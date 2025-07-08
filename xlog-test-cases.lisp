;;; File: xlog-test-cases.lisp
;;; Description: Test cases for the XLog logging library.
;;; Demonstrates logging to one, two, and three streams.

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings

;; Ensure the xlg package is available
(in-package #:xlg) ; Corrected package name to :xlg

;; --- Test Case 1: Logging to a single stream ---
(format t "~%--- Running Test Case 1: Single Stream Logging ---~%")
(with-open-log-files ((single-log "single-stream.log"))
  ;; Now pass the variable 'single-log' directly, not the keyword ':single-log'
  (xlg single-log "This is the first message to a single log stream.")
  (xlg single-log "Another message for the single log stream: ~a" "Hello, World!")
  (format t "Messages written to single-stream.log~%"))

;; --- Test Case 2: Logging to two streams ---
(format t "~%--- Running Test Case 2: Two Stream Logging ---~%")
(with-open-log-files ((app-log "two-streams-app.log")
                      (err-log "two-streams-error.log"))
  ;; Pass the variables 'app-log' and 'err-log' directly
  (xlg app-log "Application event: User login successful for ~a" "Bill")
  (xlg err-log "Error encountered: Failed to connect to database.")
  (xlg app-log "Application event: Data processed successfully.")
  (format t "Messages written to two-streams-app.log and two-streams-error.log~%"))

;; --- Test Case 3: Logging to three streams ---
(format t "~%--- Running Test Case 3: Three Stream Logging ---~%")
(with-open-log-files ((main-log "three-streams-main.log")
                      (debug-log "three-streams-debug.log")
                      (audit-log "three-streams-audit.log"))
  ;; Pass the variables 'main-log', 'debug-log', and 'audit-log' directly
  (format t "I'm wondering what type e.g., main-log ~s is~%" main-log)
  (xlg main-log "System initialization started.")
  (xlg debug-log "Debug: Configuration loaded from ~a" "/etc/config.ini")
  (xlg audit-log "Audit: Access granted to IP ~a at ~a" "192.168.1.100" (get-universal-time))
  (xlg main-log "Processing user request: ~a" "fetch_data")
  (xlg debug-log "Debug: Query executed in ~a ms" 150)
  (xlg audit-log "Audit: Data read by user ~a" "admin")
  (xlg main-log "System shutdown initiated.")
  (format t "Messages written to three-streams-main.log, three-streams-debug.log, and three-streams-audit.log~%"))

(format t "~%--- All test cases completed. Check the generated log files. ---~%")
