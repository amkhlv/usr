#!/usr/bin/env racket

#lang at-exp racket

(require bystroTeX/utils)

(define list-sockets (make-parameter #f))
(define kill (make-parameter #f))
(define fileopen (make-parameter #f))
(define sock (make-parameter #f))

(define-syntax (mkparser hs)
  (datum->syntax
   hs
   `(command-line
     #:program "Emacs server interaction"
     #:usage-help ,(apply string-append (cdr (syntax->datum hs)))
     #:once-each
     [("-l" "--list-sockets") "list Emacs sockets" (list-sockets #t)]
     [("-k" "--kill") k "kill socket" (kill k)]
     [("-o" "--open") f "open file (needs socket name!)" (fileopen f)]
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


      
