#!/usr/bin/env racket

#lang racket/base

(require racket/date racket/cmdline racket/system racket/string racket/list)

(define logfilename (make-parameter "/tmp/amkhlv-screensaver.log"))
(define errfilename (make-parameter "/tmp/amkhlv-screensaver.err"))
(define lock-pid-filename (make-parameter "/tmp/amkhlv-screensaver_xtrlock.pid"))
(define pipename (make-parameter "/root/.amkhlv-pipes/screensaver.fifo"))
(define mountpoint (make-parameter "/root/.amkhlv-unlock/"))
(define checksum-file (make-parameter "/root/.amkhlv-sha256.txt"))
(define default-lock-com '("xtrlock" "-b"))
(define lock-com '())
(define do-stop (make-parameter #f))

(set!
 lock-com
 (command-line
  #:once-each
  [("-p" "--pipe") pipe-file-name "Pipe to listen on" (pipename pipe-file-name)]
  [("-l" "--log") log-file-name "Log file path" (logfilename log-file-name)]
  [("-e" "--err") err-file-name "Errors file path" (errfilename err-file-name)]
  [("-m" "--mountpoint") m "Mount point" (mountpoint m)]
  [("-c" "--checksum-file") cf "File containing the checksum" (checksum-file cf)]
  ["--pid-file" pid-file-name "Name of file to contain the PID of the xtrlock command" (lock-pid-filename pid-file-name)]
  ["--stop-screensaver" "Stop the screensaver daemon" (do-stop #t)]
  #:usage-help "\nlisten on the pipe for the command to lock or unlock screen\n"
  #:args lock-command-and-its-args
  lock-command-and-its-args))

(unless (cons? lock-com) (set! lock-com default-lock-com))

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
(define (notify-send title text #:icon icon)
  (let ([xs 
         (process (string-append "notify-send "
                                 "-i "
                                 icon
                                 " "
                                 title
                                 " \""
                                 text
                                 "\" "))])
    (close-input-port (car xs))
    (close-output-port (cadr xs))
    (close-input-port (cadddr xs))
    (cadddr (cdr xs)))
  )

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
          (close-output-port outp)
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
                        (display (string-append (print-date) ": exiting the locking process\n") log-port))]
      [else (display (string-append (print-date) ": already exited the locking process\n") log-port)]))
  (close-output-port log-port)
  (close-output-port err-port)
  (let ([sub-control
         (notify-send "amkhlv-screenlock" "exiting" #:icon "face-sick")])
    (display (string-append "notification is " (symbol->string (sub-control 'status))))
    (sub-control 'wait)
    ))
(define (proceed-with-error c r)
  (let ([error-message (string-append "received " c)])
    (display error-message)
    (write error-message err-port)
    (notify-send "Error"  error-message #:icon "face-worried"))
  (mainloop r))
(define (mount-pendrive)
  (let-values ([(mount-proc inp outp errp)
                (subprocess log-port #f err-port (find-executable-path "mount") (mountpoint))
                ])
    (close-output-port outp)
    (subprocess-wait mount-proc)
    ))
(define (unmount-pendrive)
  (let-values ([(unmount-proc inp outp errp)
                (subprocess log-port #f err-port (find-executable-path "umount") (mountpoint))
                ])
    (close-output-port outp)
    (subprocess-wait unmount-proc)
    ))
(define (sha256sum-is-OK?)
  (mount-pendrive)
  (let-values ([(sha-proc inp-from-sha outp-to-sha err-from-sha)
                (subprocess #f #f err-port (find-executable-path "sha256sum") (string-append (mountpoint) "/unl.txt"))
                ]
               [(cat-proc inp-from-cat outp-to-cat err-from-cat)
                (subprocess #f #f err-port (find-executable-path "cat") (checksum-file))]
               )
    (let ([sha-is (car (string-split (read-line inp-from-sha)))]
          [sha-should-be (car (string-split (read-line inp-from-cat)))])
      (display (string-append "SHA256 at " (print-date) ": " sha-is "  ; should be: " sha-should-be "\n") log-port)
      (close-input-port inp-from-cat)
      (close-output-port outp-to-cat)
      (close-input-port inp-from-sha)
      (close-output-port outp-to-sha)
      (unmount-pendrive)
      (string=? sha-is sha-should-be))))
(define (mainloop lockrun)
  (let ([com (read-from-pipe)])
    (cond
      [(regexp-match? #rx"unlock" com) 
       (if (subprocess? lockrun)
         (let ([s (subprocess-status lockrun)])
           (case s
             [(running) 
              (if (sha256sum-is-OK?) 
                  (kill-xtrlock lockrun)
                  (mainloop lockrun))]
             [else 
              (display 
               (string-append 
                (print-date) 
                ": already exited the locking process with status " 
                (number->string s)
                "\n") 
               log-port)
              (mainloop #f)]))
         (mainloop #f))]
      [(regexp-match? #rx"lock" com) 
       (if (subprocess? lockrun)
           (let ([s (subprocess-status lockrun)])
             (case s
               [(running)
                (display 
                 (string-append 
                  (print-date) 
                  ": the locking process is already running \n" 
                  )
                 log-port)
                (mainloop lockrun)]
               [else 
                (launch-xtrlock lockrun)]))
           (begin
             (launch-xtrlock lockrun)))]
      [(regexp-match? #rx"exit" com) (finalize lockrun)]
      [else (proceed-with-error com lockrun)]
      )))
(if (do-stop)
    (write-to-a-file (pipename) 'exit #:mode 'text #:exists 'append)
    (begin
      (notify-send "amkhlv-screenlock" "starting" #:icon "face-angel")
      (mainloop #f)))
    
