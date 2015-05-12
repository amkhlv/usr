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

(require racket/cmdline 
         racket/set
         racket/port
         (prefix-in the: xml) 
         xml/path 
         (planet amkhlv/bystroTeX/utils) 
         (planet amkhlv/truques/terminal))

(define (charbutton x) (ansi-bold (ansi-bg256 76 (ansi-fg256 232 x))))
(define (loginbutton x) (ansi-bold (ansi-bg256  0 (ansi-fg256 82 x)))) 
(define (urlbutton x) (ansi-bold (ansi-bg256 33 (ansi-fg256 88 x))))
(define (exitbutton x) (ansi-bold (ansi-bg256 218 (ansi-fg256 18 x))))
(define (warningbutton x) (ansi-bold (ansi-bg256 218 (ansi-fg256 18 x))))
(define (high-yellow x) (ansi-bg256 228 (ansi-fg256 0 x)))
(define (strong-warning x) (ansi-bold (ansi-bg256 1 (ansi-fg256 15 x))))
(define spacebutton (ansi-bold (ansi-reverse "SPACE")))

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

(define current-passphrase (make-parameter ""))
(define current-root-xexpr (make-parameter '()))
(define current-pwds (make-parameter '()))

(define current-filename (make-parameter "passwords.gpg"))
(define filename
  (command-line #:args (current-filename) current-filename))

(define (escape)
  (abort-current-continuation
   (default-continuation-prompt-tag)
   (lambda ()
     'escaped)))

(define (request-keypress msg char)
  (display msg)
  (let ([x (get-one-char)])
    (cond
     [(char=? x char) #t]
     [(char=? x #\n) #f]
     [else (printf "\npressed -->~a<-- try again\n" x) (request-keypress msg char)])))

(define (insert-into-xsel s)
  (with-external-command-as 
   xsel "xsel" ("-i")
   (display s xsel-stdin)))

(define (read-line-from-xsel)
  (with-external-command-as 
   xsel "xsel" ()
   (read-line xsel-stdout)))

(define (load-xexpr-from-file)
  (display "\nWAIT...\n")
  (current-root-xexpr
   (with-external-command-as 
    gpg "gpg" ("--batch" "--passphrase-fd" "0" "--decrypt" filename)
    (display (current-passphrase) gpg-stdin)
    (close-output-port gpg-stdin)
    (for ([e (port->lines gpg-stderr)]) (display e))
    (the:xml->xexpr (the:read-xml/element gpg-stdout))))
  )

(define (xexpr->pwdata a site-nick site-url)
  (pwdata
   site-nick
   site-url
   (se-path* '(account #:login) a)
   (apply string-append (se-path*/list '(account password) a))
   (apply string-append (se-path*/list '(account description) a))
   (apply string-append (se-path*/list '(account notes) a))
   (apply string-append (se-path*/list '(account login_challenge) a))
   (apply string-append (se-path*/list '(account forgot_password_challenge) a))
   (apply string-append (se-path*/list '(account secret_notes) a))))

(define (load-list-of-pwdata some-xexpr)
  ((λ (x) (current-pwds (apply append x)))
   (for/list ([s (se-path*/list '(sites) some-xexpr)] #:when (cons? s))
     (for/list ([a (se-path*/list '(site) s)] #:when (cons? a))
       (xexpr->pwdata a (se-path* '(site #:nick) s) (se-path* '(site #:url) s))))))

(define (get-matches rgx)
  (let* ([ms 
          (filter 
           (lambda (x) (regexp-match rgx (pwdata-nick x)))
           (current-pwds))]
         [nicks
          (list->set (for/list ([x ms]) (pwdata-nick x)))])
    (make-hash
     (for/list ([nick nicks])
       (cons nick 
             (let ([accts 
                    (filter (lambda (x) (string=? (pwdata-nick x) nick)) ms)])
               accts))))))
    
(define (suggest-matches h)
  (define (p_enumerated xs acc)
    (let ([l (length acc)])
      (if (and (cons? xs) (l . < . 9))
          (p_enumerated (cdr xs) (cons (cons l (car xs)) acc))
        acc)))
  (define accts 
    (apply 
     append
     (for/list ([nick (hash-keys h)]) (hash-ref h nick))))
  (define accts_e (make-hash (reverse (p_enumerated accts '()))))
  (display "\n--------------\n")
  (for ([k (hash-keys accts_e)])
    (printf "~a:  ~a   ~a   ~a\n" 
            (charbutton (string #\space (integer->char (+ 97 k)) #\space))
            (pwdata-nick (hash-ref accts_e k)) 
            (urlbutton (pwdata-url  (hash-ref accts_e k)))
            (loginbutton (pwdata-login (hash-ref accts_e k))))
    (printf "~a\n\n" (pwdata-description (hash-ref accts_e k))))
  (let askhint ([msg "\n--------------\nEnter letter: "])
    (printf msg)
    (define ch (- (char->integer (get-one-char)) 97))
    (if (member ch (hash-keys accts_e)) 
        (show-data (hash-ref accts_e ch))
        (askhint "\n ERROR: Letter out of range !\n"))))


(define (show-data p)
  (ansi-clear-screen)
  (printf
   "\n~a -> ~a\n\n"
   (urlbutton (pwdata-url p))
   (loginbutton (pwdata-login p)))
  (unless (equal? "" (pwdata-description p))
    (printf 
     (string-append (ansi-underline "\nDescription:") "  ~a\n")
     (pwdata-description p)))
  (unless (equal? "" (pwdata-login-challenge p))
    (printf
     (string-append (ansi-underline "\nLogin challenge:") "  ~a\n")
     (pwdata-login-challenge p)))
  (unless (equal? "" (pwdata-notes p))
    (printf
     (string-append (ansi-underline "\nNotes:") "  ~a\n")
     (pwdata-notes p)))
  (unless (equal? "" (pwdata-forgot-password-challenge p)) (display "\n\n+forgot password challenge\n"))
  (unless (equal? "" (pwdata-secret-notes p)) (display "\n\n+secret notes\n"))
  (insert-into-xsel (pwdata-password p))
  (display (strong-warning "password copied"))
  (request-keypress 
   (format "\nPress ~a to copy login and return to mainloop\n" spacebutton) 
   #\space)
  (insert-into-xsel (pwdata-login p))
  (ansi-clear-screen))

(define decrypt-file
  (let ([f (λ (x)
             (define fn (expand-user-path x))
             (printf "\nDo you want to decrypt this file: -->~a<--\n" fn)
             (when (request-keypress 
                    (format "\nPress ~a to confirm or n to cancel\n" spacebutton) 
                    #\space)
               (let ([lines-of-plaintext
                      (with-external-command-as
                       gpg "gpg" ("--batch" "--passphrase-fd" "0" "--decrypt" fn)
                       (display (current-passphrase) gpg-stdin)
                       (close-output-port gpg-stdin)
                       (display (port->string gpg-stderr))
                       (port->string gpg-stdout))])
                 (display (high-yellow lines-of-plaintext))
                 (request-keypress 
                  (format "\n\nPress ~a to return to mainloop\n" spacebutton) 
                  #\space)))
             (ansi-clear-screen))])
    (case-lambda 
      [() (f (read-line-from-xsel))]
      [(x) (f x)])))

(define (reload)
  (load-xexpr-from-file)
  (load-list-of-pwdata (current-root-xexpr))
  (display "\ndata seems OK\n"))

(define (nothing-found-info r)
  (ansi-clear-screen)
  (display (warningbutton (string-append "Nothing found for -->" r "<--\n\n\n"))))

(ansi-clear-screen)
(display "Enter passphrase:")
(current-passphrase (askpass))
(display "\nthank you\n")
(reload)

(let mainloop ()
  (printf "Enter ~a, or ~a to decrypt a file, or:\n" (ansi-underline "regexp") (ansi-underline "!path/to/file.gpg"))
  (printf "~a to decrypt\n" (charbutton "d"))
  (printf "~a or ~a to exit\n" (charbutton "q") (charbutton "e"))
  (printf "~a to reload\n" (charbutton "r"))
  (let ([r (read-line)])
    (cond
     [(string=? r "") (ansi-clear-screen) (mainloop)]
     [(string=? r "d") (decrypt-file) (mainloop)]
     [(char=? (string-ref r 0) #\!) (decrypt-file (substring r 1)) (mainloop)]
     [((string=? r "e") . or . (string=? r "q")) (display (exitbutton "Exiting\n"))]
     [(string=? r "r") (ansi-clear-screen) (reload) (mainloop)]
     [else ;treat as regular expression
      (let [(ms (get-matches r))]
        (if (cons? (hash-keys ms))
            (begin (suggest-matches ms)   (mainloop))
            (begin (nothing-found-info r) (mainloop))))])))
