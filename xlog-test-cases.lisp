;;; File: xlog-test-cases.lisp
;;; Description: Test cases for the XLog logging library.
;;; Demonstrates logging to one, two, and three streams, including date prefixes.

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0))) ; Debugging optimization settings

;; Ensure the xlg package is available
(in-package #:xlg)

;; --- Test Case 1: Logging to a single stream with YMD date prefix for filename and microsecond line prefix ---
(format t "~%--- Running Test Case 1: Single Stream Logging with YMD Filename and Microsecond Line Prefixes ---~%")
(with-open-log-files ((single-log "single-stream.log" :ymd)) ; Filename prefixed with YMD
  ;; Line prefixed with microsecond timestamp, no additional string
  (xlg single-log "This is the first message to a single log stream with date prefix." :line-prefix "")
  ;; Line prefixed with microsecond timestamp and a custom string
  (xlg single-log "Another message for the single log stream: ~a" "Hello, Microseconds!" :line-prefix "[SEC-LOG] ") ; Modified line
  (format t "Messages written to date-prefixed single-stream.log~%"))

;; --- Test Case 2: Logging to two streams, one with YMD prefix, one without, and varied line prefixes ---
(format t "~%--- Running Test Case 2: Two Stream Logging (One Filename Prefix, Varied Line Prefixes) ---~%")
(with-open-log-files ((app-log "two-streams-app.log" :ymd) ; Filename prefixed with YMD
                      (err-log "two-streams-error.log"))    ; No filename prefix
  ;; Line prefixed with microsecond timestamp and a custom string
  (xlg app-log "Application event: User login successful for ~a (date-prefixed filename and line)" "Bill" :line-prefix "[APP] ")
  ;; Line prefixed with microsecond timestamp, no additional string
  (xlg err-log "Error encountered: Failed to connect to database (no filename prefix, but line has microsecond date)." :line-prefix "[SEC-LOG] ") ; Modified line
  ;; No line prefix
  (xlg app-log "Application event: Data processed successfully (date-prefixed filename, no line prefix)." "date-prefixed filename, no line prefix")
  ;; No line prefix
  (xlg err-log "Another error message (no filename prefix, no line prefix).")
  (format t "Messages written to date-prefixed two-streams-app.log and two-streams-error.log~%"))

;; --- Test Case 3: Logging to three streams, all with YMD prefix for filename and varied line prefixes ---
(format t "~%--- Running Test Case 3: Three Stream Logging with YMD Filename and Varied Line Prefixes ---~%")
(with-open-log-files ((main-log "three-streams-main.log" :ymd)   ; :ymd filename prefix
                      (debug-log "three-streams-debug.log" :ymd) ; :ymd filename prefix
                      (audit-log "three-streams-audit.log" :ymd)) ; :ymd filename prefix
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg main-log "System initialization started (main log, filename & line prefixed)." :line-prefix "[MAIN] ")
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg debug-log "Debug: Configuration loaded from ~a (debug log, filename & line prefixed)" "/etc/config.ini" :line-prefix "[SEC-LOG] ") ; Modified line
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg audit-log "Audit: Access granted to IP ~a at ~a (audit log, filename & line prefixed)" "192.168.1.100" (get-universal-time) :line-prefix "[AUDIT] ")
  (xlg audit-log "Debug: Configuration loaded from ~a (debug log, filename & line prefixed)" "radio" "radio" "wherefor art thou radio" :line-prefix ) ; Modified line
  ;; No line prefix
  (xlg main-log "Processing user request: ~a (main log, filename prefixed, no line prefix)" "fetch_data")
  ;; No line prefix
  (xlg debug-log "Debug: Query executed in ~a ms (debug log, filename prefixed, no line prefix)" 150)
  ;; No line prefix
  (xlg audit-log "Audit: Data read by user ~a (audit log, filename prefixed, no line prefix)" "admin")
  ;; Line prefixed with microsecond timestamp and custom string
  (xlg main-log "System shutdown initiated (main log, filename & line prefixed)." :line-prefix "[MAIN] ")
  (format t "Messages written to date-prefixed three-streams-main.log, debug.log, and audit.log~%"))

(format t "~%--- All test cases completed. Check the generated log files. ---~%")
