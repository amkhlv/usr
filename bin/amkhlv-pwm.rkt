#!/usr/local/bin/racket

#lang racket

#|
The single command line argument is the name of the encrypted XML file;
it defaults to "passwords.gpg". The contents are of the form:
<xml>
  <sites>
    <site nick="facebook" url="facebook.com">
      <account login="my@email.com" password="secretword">My Facebook account</account>
    </site>
    ...
  </sites>
</xml>
|#

(require (planet neil/charterm) racket/cmdline (prefix-in the: xml) xml/path)

(struct pwdata (nick url login password description))

(define current-filename (make-parameter "passwords.gpg"))
(define filename
  (command-line #:args (current-filename) current-filename))

(define (escape)
  (abort-current-continuation
   (default-continuation-prompt-tag)
   (lambda ()
     'escaped)))

(define (myreadline #:hide [h #f])
  (let nextchar ([key (charterm-read-key)]
                 [acc ""])
    (if (char? key)
        (begin
          (if h (charterm-display "*") (charterm-display key))
          (nextchar (charterm-read-key)
                    (string-append acc (string key))))
        (begin
          (when (eq? 'escape key)
            (escape))
          (if (eq? 'backspace key)
              (begin
                (charterm-display (integer->char 8)) ; this is the backspace
                (nextchar (charterm-read-key)
                          (substring acc 0 (- (string-length acc) 1))))
              (begin
                (charterm-display (symbol->string key))
                acc))
          ))))

(define (insert-into-xsel s)
  (let-values ([(proc inp outp errp)
                (subprocess #f #f #f
                            (find-executable-path "xsel")
                            "-i")])
    (display s outp)
    (close-input-port inp)
    (close-output-port outp)
    (close-input-port errp)))

(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 10 5)
 (charterm-display "Enter passphrase: ")
 (define passphrase
   (myreadline #:hide #t))
 (define-values (proc inp outp errp)
   (subprocess #f #f #f
               (find-executable-path "gpg")
               "--batch"
               "--passphrase-fd"
               "0"
               "--decrypt"
               filename))
 (display passphrase outp)
 (close-output-port outp)
 (define xpr (the:xml->xexpr (the:read-xml/element inp)))
 (for/list ([ln (string-split (port->string errp) #rx"\n")])
   (charterm-newline)
   (charterm-display ln))
 (sleep 1)
 (close-input-port inp)
 (close-input-port errp)
 (define (display-account-info a)
   (insert-into-xsel (pwdata-password a))
   (charterm-clear-screen)
   (charterm-cursor 10 5)
   (charterm-display (pwdata-login a))
   (charterm-cursor 10 10)
   (charterm-display (pwdata-description a))
   )
 (define (show-matches rgx)
   (charterm-clear-screen)
   (let ([pwhash ; int -> pwdata
          (make-hash
           (let prehash ; this is a list of pairs
               ([j 0]
                [remaining
                 (filter (lambda (pattern)
                           (and
                            (cons? pattern)
                            (or
                             (regexp-match rgx (se-path* '(account) pattern))
                             (regexp-match rgx (se-path* '(site #:nick) pattern)))))
                         (se-path*/list '(sites) xpr))]
                [accumulated '()])
             (if (cons? remaining)
                 (let ([xn (pwdata
                            (se-path* '(site #:nick) (car remaining))
                            (se-path* '(site #:url) (car remaining))
                            (se-path* '(account #:login) (car remaining))
                            (se-path* '(account #:password) (car remaining))
                            (se-path* '(account) (car remaining)))])
                   (when (< j 10)
                     (charterm-bold)
                     (charterm-display (string (integer->char (+ 97 j))))
                     (charterm-normal)
                     (charterm-display (string-append ": [" (pwdata-nick xn) "](" (pwdata-url xn) ")"))
                     (charterm-newline)
                     (charterm-display (string-append "   " (pwdata-login xn)))
                     (charterm-underline)
                     (charterm-display "   ")
                     (charterm-normal)
                     (charterm-display (pwdata-description xn))
                     (charterm-newline)
                     (charterm-display "----------------------------")
                     (charterm-newline))
                   (prehash (+ j 1) (cdr remaining) (cons (cons j xn) accumulated)))
                 accumulated)))])
     (let ([kk (charterm-read-key)])
       (when (char? kk)
         (let ([k (- (char->integer kk) 97)])
           (when (hash-has-key? pwhash k)
             (display-account-info (hash-ref pwhash k))))))))
 (define (mainloop)
   (charterm-newline)
   (charterm-underline)
   (charterm-display "Enter regex:")
   (charterm-normal)
   (let ([r (myreadline)]) (show-matches (regexp r)))
   (mainloop)
   )
 (call-with-continuation-prompt mainloop)
 (display "EXITING\n")
 )
