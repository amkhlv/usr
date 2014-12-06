#!/usr/local/bin/racket

#lang racket

#|
The single command line argument is the name of the encrypted XML file;
it defaults to "passwords.gpg". The contents are of the form:
<xml>
  <sites>
    <site nick="facebook" url="facebook.com">
      <account login="my@email.com">
        <password>mysecretword</password>
        <description>My Facebook account</description>
        <notes>maybe extra info</notes>
        <login_challenge>additional challenge questions needed to login, if any</login_challenge>
        <forgot_password_challenge>what to do if forgot password</forgot_password_challenge>
        <secret_notes>maybe some secret notes</secret_notes>
      </account>
    </site>
    ...
  </sites>
</xml>
|#

(require (planet neil/charterm) racket/cmdline (prefix-in the: xml) xml/path)

(struct pwdata (nick
                url
                login
                password
                description
                notes
                login-challenge
                forgot-password-challenge
                secret-notes
                ))

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
              (if (> (string-length acc) 0)
                  (begin
                    (charterm-display (integer->char 8)) ; this is the backspace
                    (nextchar (charterm-read-key)
                              (substring acc 0 (- (string-length acc) 1))))
                  (nextchar (charterm-read-key) ""))
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
   (charterm-cursor 2 3)
   (charterm-display (pwdata-login a))
   (charterm-cursor 2 7)
   (charterm-display (pwdata-description a))
   (charterm-newline)
   (charterm-newline)
   (when (pwdata-login-challenge a)
     (charterm-bold)
     (charterm-clear-line-left)
     (charterm-display "+ login challenge:")
     (charterm-normal)
     (charterm-newline)
     (charterm-display (pwdata-login-challenge a))
     (charterm-newline))
   (when (pwdata-notes a)
     (charterm-clear-line-left)
     (charterm-display "+ notes:")
     (charterm-newline)
     (charterm-display (pwdata-notes a))
     (charterm-newline))
   (charterm-newline)
   (charterm-clear-line-left)
   (when (pwdata-forgot-password-challenge a)
     (charterm-display "+ forgot-password options "))
   (when (pwdata-secret-notes a)
     (charterm-display "+ secret notes "))
   (charterm-newline)
   (charterm-newline)
   (charterm-clear-line-left)
   (charterm-underline)
   (charterm-display "press F4 to insert the login and proceed")
   (charterm-normal)
   (charterm-newline)
   (charterm-clear-line-left)
   (define (wait-for-f4)
     (let ([k (charterm-read-key)])
       (if (eq? 'f4 k) (insert-into-xsel (pwdata-login a)) (wait-for-f4))))
   (wait-for-f4)
   (charterm-clear-screen)
   (when (pwdata-login-challenge a)
     (charterm-bold)
     (charterm-clear-line-left)
     (charterm-display "+ login challenge:")
     (charterm-normal)
     (charterm-newline)
     (charterm-display (pwdata-login-challenge a))
     (charterm-newline))
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
                            (se-path* '(account password) (car remaining))
                            (se-path* '(account description) (car remaining))
                            (se-path* '(account notes) (car remaining))
                            (se-path* '(account login_challenge) (car remaining))
                            (se-path* '(account forgot_password_challenge) ( car remaining))
                            (se-path* '(account secret_notes) (car remaining)))])
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
   (charterm-newline)
   (charterm-clear-line-left)
   (charterm-underline)
   (charterm-display "Enter regex:")
   (charterm-normal)
   (let ([r (myreadline)]) (show-matches (regexp r)))
   (mainloop)
   )
 (call-with-continuation-prompt mainloop)
 (charterm-newline)
 (charterm-display "EXITING")
 (charterm-newline)
 )
