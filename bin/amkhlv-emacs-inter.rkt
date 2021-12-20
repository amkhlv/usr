#!/usr/bin/env racket

#lang at-exp racket

(require bystroTeX/utils)

(define list-sockets (make-parameter #f))
(define kill (make-parameter #f))
(define fileopen (make-parameter #f))
(define sock (make-parameter #f))
(define revive (make-parameter #f))

(define-syntax (mkparser hs)
  (datum->syntax
   hs
   `(command-line
     #:program "Emacs server interaction"
     #:usage-help ,(apply string-append (cdr (syntax->datum hs)))
     #:once-each
     [("-k" "--kill") k "kill socket" (kill k)]
     [("-l" "--list-sockets") "list Emacs sockets" (list-sockets #t)]
     [("-o" "--open") f "open file (needs socket name!)" (fileopen f)]
     [("-r" "--revive") r "\"revive\" the socket (needs socket name!)" (revive #t)]
     [("-s" "--socket") s "socket name" (sock s)]
     )))

@mkparser{
        _____________________________________________________________________________
          Emacs server interaction
          _____________________________________________________________________________
          }

(define (show-sockets in)
  (define (main)
    (let ([ln (read-line in)])
      (if (eof-object? ln)
          (close-input-port in)
          (let ([m (regexp-match #px"/run/user/[0-9]+/emacs/([^[:space:]]+)" ln)])
            (when m (displayln (cadr m)))
            (main)
            )
          )
      ))
  (main))

(when (list-sockets)
  (with-subprocess-as
    lsof #f #f (current-error-port)
    ("lsof" "-U" "+E")
    (with-subprocess-as
      grep1 #f lsof-stdout (current-error-port)
      ("grep" "emacs")
      (with-subprocess-as
        grep2 #f grep1-stdout (current-error-port)
        ("grep" "-F" "/run/user")
        (show-sockets grep2-stdout)))))
    
(when (kill)
  (with-subprocess-as
    emacs-killer
    #f
    #f
    (current-error-port)
    ("emacsclient" "-s" (kill) "-e" "(kill-emacs)")))

(when (fileopen)
  (displayln (format "Opening file ~a with emacs server ~a" (fileopen) (sock)))
  (with-subprocess-as
    emacs-opener
    #f
    #f
    (current-error-port)
    ("emacsclient" "-s" (sock) (fileopen))
    (subprocess-wait emacs-opener-process)
    ))
      
(when (revive)
  (displayln (format "Creating window for socket ~a" (sock)))
  (with-subprocess-as
    emacs-reanimator
    #f
    #f
    (current-error-port)
    ("emacsclient" "-c" "-s" (sock))
    (subprocess-wait emacs-reanimator-process)
    ))
