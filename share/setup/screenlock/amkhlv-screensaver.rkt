#!/usr/bin/env racket

#lang racket/base

(require racket/date racket/cmdline)

(define logfilename (make-parameter "/tmp/amkhlv-screensaver.log"))
(define errfilename (make-parameter "/tmp/amkhlv-screensaver.err"))
(define lock-pid-filename (make-parameter "/tmp/amkhlv-screensaver_xtrlock.pid"))
(define pipename (make-parameter "pipe.fifo"))
(define lock-com '("xtrlock" "-b"))
(define do-stop (make-parameter #f))

(set!
 lock-com
 (command-line
  #:once-each
  [("-p" "--pipe") pipe-file-name "Pipe to listen on" (pipename pipe-file-name)]
  [("-l" "--log") log-file-name "Log file path" (logfilename log-file-name)]
  [("-e" "--err") err-file-name "Errors file path" (errfilename err-file-name)]
  ["--pid-file" pid-file-name "Name of file to contain the PID of the xtrlock command" (lock-pid-filename pid-file-name)]
  ["--stop-screensaver" "Stop the screensaver daemon" (do-stop #t)]
  #:usage-help "\nlisten on the pipe for the command to lock or unlock screen\n"
  #:args lock-command-and-its-args
  lock-command-and-its-args))

(define log-port (open-output-file (logfilename) #:mode 'text #:exists 'append))
(define err-port (open-output-file (errfilename) #:mode 'text #:exists 'append))
(define (print-date)
  (let ([d (current-date)])
    (string-append
     (number->string (date-year d))
     "-"
     (number->string (date-month d))
     "-"
     (number->string (date-day d))
     " at "
     (number->string (date-hour d))
     ":"
     (number->string (date-minute d))
     "."
     (number->string (date-second d)))))
(define (write-to-a-file path txt #:mode m #:exists e)
  (call-with-output-file path
    (lambda (output-port) (write txt output-port)) #:mode m #:exists e))
(define (read-from-pipe)
  (let ([inp (open-input-file (pipename))])
    (define x (read-line inp))
    (close-input-port inp)
    x))
(define (launch-xtrlock r)
  (display (string-append (print-date) ": locking\n") log-port)
  (flush-output log-port)
  (define newp
    (if (subprocess? r)
        r
        (let-values ([(newlock-proc inp outp errp)
                      (apply subprocess log-port #f err-port (find-executable-path (car lock-com)) (cdr lock-com))
                      ])
          newlock-proc)))
  (write-to-a-file (lock-pid-filename) (subprocess-pid newp) #:mode 'text #:exists 'replace)
  (mainloop newp))
(define (kill-xtrlock r)
  (display (string-append (print-date) ": unlocking\n") log-port)
  (flush-output log-port)
  (when (subprocess? r)
    (subprocess-kill r #t))
  (mainloop #f))
(define (finalize r)
  (display (string-append (print-date) ": finalizing\n") log-port)
  (when (subprocess? r)
    (case (subprocess-status r)
      [(running) (begin (subprocess-kill r #t)
                        (display (string-append (print-date) ": exiting\n") log-port))]
      [else (display (string-append (print-date) ": already exited\n") log-port)]))
  (close-output-port log-port)
  (close-output-port err-port))
(define (mainloop lockrun)
  (let ([com (read-from-pipe)])
    (case com
      [("lock") (launch-xtrlock lockrun)]
      [("unlock") (kill-xtrlock lockrun)]
      [("exit") (finalize lockrun)]
      [else (display (string-append "ERROR: received <<" com ">>"))]
      )))
(if (do-stop)
    (write-to-a-file (pipename) 'exit #:mode 'text #:exists 'append)
    (mainloop #f))
    
