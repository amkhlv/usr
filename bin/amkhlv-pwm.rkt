#!/usr/bin/env racket

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

(define (myreadline #:hide [hide #f])
  (let nextchar ([key (charterm-read-key)]
                 [acc ""])
    (if (char? key)
        (begin
          (if hide (charterm-display "*") (charterm-display key))
          (nextchar (charterm-read-key)
                    (string-append acc (string key))))
        (case key
          [(escape) (escape)]
          [(f5) (begin (read-xpr-from-file) 'reload)]
          [(f6) (begin (decrypt-file) 'decrypt-file)]
          [(backspace)
           (if (> (string-length acc) 0)
               (begin
                 (charterm-display (integer->char 8)) ; this is the backspace
                 (nextchar (charterm-read-key)
                           (substring acc 0 (- (string-length acc) 1))))
               (nextchar (charterm-read-key) ""))]
          [else (begin
                  (charterm-display (symbol->string key))
                  acc)]
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

(define passphrase "")
(define xpr '())

(define (read-xpr-from-file)
  (charterm-clear-screen)
  (charterm-display "WAIT...")
  (charterm-newline)
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
  (set! xpr (the:xml->xexpr (the:read-xml/element inp)))
  (for/list ([ln (string-split (port->string errp) #rx"\n")])
    (charterm-newline)
    (charterm-clear-line)
    (charterm-display ln))
  (sleep 1)
  (close-input-port inp)
  (close-input-port errp))

(define (decrypt-file)
  (charterm-newline)
  (charterm-display "ENTER NAME OF FILE TO DECRYPT:")
  (charterm-newline)
  (define fn (expand-user-path (myreadline #:hide #f)))
  (charterm-newline)
  (charterm-display fn)
  (charterm-newline)
  (define-values (proc inp outp errp)
    (subprocess #f #f #f
                (find-executable-path "gpg")
                "--batch"
                "--passphrase-fd"
                "0"
                "--decrypt"
                fn))
  (display passphrase outp)
  (close-output-port outp)
  (for/list ([ln (string-split (port->string errp) #rx"\n")])
    (charterm-newline)
    (charterm-clear-line)
    (charterm-display ln))
  (charterm-newline)
  (for/list ([ln (string-split (port->string inp) #rx"\n")])
    (charterm-newline)
    (charterm-clear-line)
    (charterm-display ln))
  (close-input-port inp)
  (close-input-port errp)
  )

(with-charterm
 (charterm-clear-screen)
 (charterm-cursor 10 5)
 (charterm-display "Enter passphrase: ")
 (set! passphrase  (myreadline #:hide #t))
 (read-xpr-from-file)
 (define (display-account-info a)
   (insert-into-xsel (pwdata-password a))
   (charterm-clear-screen)
   (charterm-cursor 2 3)
   (charterm-display (pwdata-login a))
   (charterm-cursor 2 7)
   (charterm-display (pwdata-description a))
   (charterm-newline)
   (charterm-newline)
   (unless (string=? (pwdata-login-challenge a) "")
     (charterm-bold)
     (charterm-clear-line-left)
     (charterm-display "+ login challenge:")
     (charterm-normal)
     (charterm-newline)
     (charterm-display (pwdata-login-challenge a))
     (charterm-newline))
   (unless (string=? (pwdata-notes a) "")
     (charterm-clear-line-left)
     (charterm-display "+ notes:")
     (charterm-newline)
     (charterm-display (pwdata-notes a))
     (charterm-newline))
   (charterm-newline)
   (charterm-clear-line-left)
   (unless (string=? (pwdata-forgot-password-challenge a) "")
     (charterm-display "+ forgot-password options "))
   (unless (string=? (pwdata-secret-notes a) "")
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
   (unless (string=? (pwdata-login-challenge a) "")
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
   (define pwhash ; int -> pwdata
     (let prehash ; this is a list of pairs
         ([j 0]
          [remaining
           (filter (lambda (a-site)
                     (and
                      (cons? a-site) ; to avoid a whitespace
                      (regexp-match rgx (se-path* '(site #:nick) a-site))
                      ))
                   (se-path*/list '(sites) xpr))]
          [accumulated (make-hash)])
       (if (and (cons? remaining) (j . < . 10))
           (begin 
             (for ([a (se-path*/list '(site) (car remaining))] #:when (cons? a))
               (let ([xn (pwdata
                          (se-path* '(site #:nick) (car remaining))
                          (se-path* '(site #:url) (car remaining))
                          (se-path* '(account #:login) a)
                          (apply string-append (se-path*/list '(account password) a))
                          (apply string-append (se-path*/list '(account description) a))
                          (apply string-append (se-path*/list '(account notes) a))
                          (apply string-append (se-path*/list '(account login_challenge) a))
                          (apply string-append (se-path*/list '(account forgot_password_challenge) a))
                          (apply string-append (se-path*/list '(account secret_notes) a)))])
                 (when (< j 10)
                   (charterm-bold)
                   (charterm-display (string (integer->char (+ 97 j))))
                   (charterm-normal)
                   (charterm-display (string-append ": [" (pwdata-nick xn) "](" (pwdata-url xn) ")"))
                   (charterm-newline)
                   (unless (string=? (pwdata-login xn) "") (charterm-display (string-append "   " (pwdata-login xn))))
                   (charterm-underline)
                   (charterm-display "   ")
                   (charterm-normal)
                   (unless (string=? (pwdata-description xn) "") (charterm-display (pwdata-description xn)))
                   (charterm-newline)
                   (charterm-display "----------------------------")
                   (charterm-newline))
                 (hash-set! accumulated j xn)
                 (set! j (+ j 1))
                 ))
             (prehash j (cdr remaining) accumulated))
           accumulated)))
   (let ([kk (charterm-read-key)])
     (when (char? kk)
       (let ([k (- (char->integer kk) 97)])
         (when (hash-has-key? pwhash k)
           (display-account-info (hash-ref pwhash k)))))))
 (define (mainloop)
   (charterm-newline)
   (define (ask-for-regexp)
     (charterm-newline)
     (charterm-clear-line-left)
     (charterm-underline)
     (charterm-display "Enter regex, or F5 for reload, or F6 to decrypt file, or ESC to quit:")
     (charterm-normal)
     (let ([r (myreadline)])
       (case r
         ((reload)
          (charterm-newline)
          (charterm-display "RELOADED")
          (ask-for-regexp))
         ((decrypt-file)
          (ask-for-regexp))
         (else (show-matches (regexp r)))
         )))
   (ask-for-regexp)
   (mainloop)
   )
 (call-with-continuation-prompt mainloop)
 (charterm-newline)
 (charterm-display "EXITING")
 (charterm-newline)
 )
