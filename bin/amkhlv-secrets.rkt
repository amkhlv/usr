#!/usr/bin/env racket

#lang racket

#|
The single command line argument is the name of the encrypted XML file;
it defaults to "passwords.gpg". The contents are of the form:
<xml>
  <sites>
    <site nick="facebook" url="facebook.com">
      <account login="my@email.com">
        <password changed="2014-06-19" expires="2015-06-19">mysecretword</password>
        <description>My Facebook account</description>
        <notes>maybe extra info</notes>
        <login_challenge>additional challenge questions needed to login, if any</login_challenge>
        <forgot_password_challenge>what to do if forgot password</forgot_password_challenge>
        <secret_notes>maybe some secret notes</secret_notes>
        <tags><tag>friends</tag><tag>fun</tag>...</tags>
      </account>
    </site>
    ...
  </sites>
</xml>

More precisely, using the Relax NG compact notations:

element xml {
   element sites {
      element site {
         attribute nick { text },
         attribute url  { text },
         element account {
            attribute login { text },
            element password { 
               attribute changed { xsd:date }?,
               attribute expires { xsd:date }?,
               text 
            },
            element description { text },
            element notes { text }?,
            element login_challenge { text }?,
            element forgot_password_challenge { text }?,
            element secret_notes { text }?,
            element tags { 
               element tag {text}*
            }?
         }*
      }+
   }}

|#

(require racket/cmdline 
         racket/set
         racket/port
         racket/date
         (prefix-in the: xml) 
         xml/path 
         (planet amkhlv/bystroTeX/utils) 
         (planet amkhlv/truques/terminal)
         (planet amkhlv/truques/xml))

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
                password-changed
                password-expires
                description
                notes
                login-challenge
                forgot-password-challenge
                secret-notes
                tags
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
   xsel ("xsel" "-i")
   (display s xsel-stdin)))

(define (read-line-from-xsel)
  (with-external-command-as 
   xsel ("xsel")
   (read-line xsel-stdout)))

(define (load-xexpr-from-file)
  (display "\nWAIT...\n")
  (current-root-xexpr
   (with-external-command-as 
    gpg ("gpg" "--batch" "--passphrase-fd" "0" "--decrypt" filename)
    (display (current-passphrase) gpg-stdin)
    (close-output-port gpg-stdin)
    (for ([e (port->lines gpg-stderr)]) (display e))
    (the:xml->xexpr (the:read-xml/element gpg-stdout))))
  )

(define (xexpr->pwdata a site-nick site-url)
  (define (f x) (and (cons? x) (apply string-append x)))
  (pwdata
   site-nick
   site-url
   (se-path* '(account #:login) a)
   (let ([x (se-path*/list '(account password) a)]) (apply string-append x))
   (let ([x (se-path*/list '(account password #:changed) a)]) (f x))
   (let ([x (se-path*/list '(account password #:expires) a)]) (f x))
   (let ([x (se-path*/list '(account description) a)]) (f x))
   (let ([x (se-path*/list '(account notes) a)]) (f x))
   (let ([x (se-path*/list '(account login_challenge) a)]) (f x))
   (let ([x (se-path*/list '(account forgot_password_challenge) a)]) (f x))
   (let ([x (se-path*/list '(account secret_notes) a)]) (f x))
   (for/list ([x (se-path*/list '(account tags) a)] #:when (cons? x)) 
     (apply string-append (se-path*/list '(tag) x)))
   ))

(define (load-list-of-pwdata some-xexpr)
  ((λ (x) (current-pwds (apply append x)))
   (for/list ([s (se-path*/list '(sites) some-xexpr)] #:when (cons? s))
     (for/list ([a (se-path*/list '(site) s)] #:when (cons? a))
       (xexpr->pwdata a (se-path* '(site #:nick) s) (se-path* '(site #:url) s))))))

(define (collect-tags ps)
  (list->set (apply append (for/list ([p ps]) (pwdata-tags p)))))

(define (hint-tags)
  (define hh (make-hash))
  (for ([t (collect-tags (current-pwds))])
    (let ([n (length (hash-keys hh))])
      (printf "~a:  ~a\n" 
              (charbutton (string #\space (integer->char (+ 97 n)) #\space)) t)
      (hash-set! hh n t)))
  hh)

(define (get-matches rgx (tags #f))
  (let* ([ms 
          (filter 
           (lambda (x) (and
                        (or
                         (not rgx)
                         (regexp-match (regexp rgx) (pwdata-nick x))
                         (regexp-match (regexp rgx) (pwdata-url x)))
                        (or (not tags)
                            (for/and ([tag tags]) (member tag (pwdata-tags x))))))
           (current-pwds))]
         [nicks (list->set (for/list ([x ms]) (pwdata-nick x)))])
    (make-hash
     (for/list ([nick nicks])
       (cons nick 
             (let ([accts 
                    (filter (lambda (x) (string=? (pwdata-nick x) nick)) ms)])
               accts))))))

(define (suggest-matches h)
  (define (p_enumerated xs acc)
    (let ([l (length acc)])
      (if (and (cons? xs) (l . < . 16))
          (p_enumerated (cdr xs) (cons (cons l (car xs)) acc))
          acc)))
  (define accts 
    (apply 
     append
     (for/list ([nick (hash-keys h)]) (hash-ref h nick))))
  (define accts_e (make-hash (reverse (p_enumerated accts '()))))
  (display "\n--------------\n")
  (for ([k (hash-keys accts_e)])
    (printf "~a── ~a   ~a   ~a   ~a\n" 
            (charbutton (string #\space (integer->char (+ 97 k)) #\space))
            (pwdata-nick (hash-ref accts_e k)) 
            (urlbutton (pwdata-url  (hash-ref accts_e k)))
            (loginbutton (pwdata-login (hash-ref accts_e k)))
            (let ([expiring (pwdata-password-expires (hash-ref accts_e k))])
              (if expiring
                  (let* ([exp-date* (xsd:date->date* expiring)]
                         [exp-seconds (date->seconds exp-date*)])
                    (if (> (+ (current-seconds) (* 30 86400)) exp-seconds)
                        (strong-warning (string-append "Expires on " expiring))
                        (exitbutton "❄")))
                  "")))
    (printf " ╰─── ~a\n" (or (pwdata-description (hash-ref accts_e k)) "---")))
  (let askhint ([msg "\n--------------\nEnter letter or ESC: "])
    (printf msg)
    (define ch (- (char->integer (get-one-char)) 97))
    (if (member ch (hash-keys accts_e)) 
        (show-data (hash-ref accts_e ch))
        (if (eqv? 27 (+ ch 97))
            (ansi-clear-screen)
            (askhint "\n ERROR: Letter out of range !\n")))))

(define (show-data p)
  (ansi-clear-screen)
  (printf
   "\n~a -> ~a\n\n"
   (urlbutton (pwdata-url p))
   (loginbutton (pwdata-login p)))
  (when (pwdata-password-changed p)
    (printf
     (string-append (ansi-underline "\nPassword changed on:") "  ~a   ")
     (pwdata-password-changed p)))
  (when (pwdata-password-expires p)
    (printf
     (string-append (ansi-underline "expires on:") "  ~a   \n")
     (pwdata-password-changed p)))
  (when (pwdata-description p)
    (printf 
     (string-append (ansi-underline "\nDescription:") "  ~a\n")
     (pwdata-description p)))
  (when (pwdata-login-challenge p)
    (printf
     (string-append (ansi-underline "\nLogin challenge:") "  ~a\n")
     (pwdata-login-challenge p)))
  (when (pwdata-notes p)
    (printf
     (string-append (ansi-underline "\nNotes:") "  ~a\n")
     (pwdata-notes p)))
  (when (pwdata-forgot-password-challenge p) 
    (printf "\n\n+forgot password challenge (press ~a to see)\n" (charbutton "c")))
  (when (pwdata-secret-notes p) 
    (printf "\n\n+secret notes (press ~a to see)\n" (charbutton "s")))
  (insert-into-xsel (pwdata-password p))
  (display (strong-warning "password copied"))
  (printf "\n\nPress ~a to copy login and return to mainloop\n" spacebutton)
  (let askhint ()
    (define ch (get-one-char))
    (cond
     [(and (char=? ch #\s) (pwdata-secret-notes p)) 
      (display (high-yellow (pwdata-secret-notes p)))
      (request-keypress 
       (printf "\n\nPress ~a to copy login and return to mainloop\n" spacebutton) 
       #\space)
      (insert-into-xsel (pwdata-login p))]
     [(and (char=? ch #\c) (pwdata-forgot-password-challenge p))
      (display (high-yellow (pwdata-forgot-password-challenge p)))
      (request-keypress 
       (printf "\n\nPress ~a to copy login and return to mainloop\n" spacebutton) 
       #\space)
      (insert-into-xsel (pwdata-login p))]
     [(char=? ch #\space) (insert-into-xsel (pwdata-login p))]
     [else (askhint)]))
  (ansi-clear-screen))

(define decrypt-file
  (let ([f (λ (x)
             (define fn (expand-user-path x))
             (printf "\nDo you want to decrypt this file: -->~a<--\n" fn)
             (when (request-keypress 
                    (format "\nPress ~a to confirm or any other key to cancel\n" spacebutton) 
                    #\space)
               (let ([lines-of-plaintext
                      (with-external-command-as
                       gpg ("gpg" "--batch" "--passphrase-fd" "0" "--decrypt" fn)
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

(define (split-on-first-comma s)
  (let a ([comma-pos 0])
    (cond
     [(not (comma-pos . < . (string-length s)))
      (cons s #f)]
     [(char=? #\, (string-ref s comma-pos))
      (cons (substring s 0 comma-pos) (substring s (+ 1 comma-pos)))]
     [else (a (+ 1 comma-pos))])))
(define (hints->tags ht s)
  (for/list ([c (string->list s)] #:when (member (- (char->integer c) 97) (hash-keys ht)))
    (hash-ref ht (- (char->integer c) 97))))

(ansi-clear-screen)
(display "Enter passphrase:")
(current-passphrase (askpass))
(display "\nthank you\n")
(reload)

(let mainloop ([with-tags #f])
  (printf "~a\n" filename)
  (if with-tags
      (printf "Enter ~a, or ~a to decrypt a file, or:\n" (ansi-underline "tags,regexp") (ansi-underline "!path/to/file.gpg"))
      (printf "Enter ~a, or ~a to decrypt a file, or:\n" (ansi-underline "regexp") (ansi-underline "!path/to/file.gpg")))
  (printf "~a to decrypt\n" (charbutton "d"))
  (printf "~a or ~a to exit\n" (charbutton "q") (charbutton "e"))
  (printf "~a to reload\n" (charbutton "r"))
  (printf "~a to show tags\n" (charbutton "t"))
  (printf "~a to copy password\n" (charbutton "p"))
  (let ([r (read-line)])
    (cond
     [(string=? r "") (ansi-clear-screen) (mainloop #f)]
     [(string=? r "d") (decrypt-file) (mainloop #f)]
     [(char=? (string-ref r 0) #\!) (decrypt-file (substring r 1)) (mainloop #f)]
     [((string=? r "e") . or . (string=? r "q")) (display (exitbutton "Exiting\n"))]
     [(string=? r "r") (ansi-clear-screen) (reload) (mainloop #f)]
     [(string=? r "t") (mainloop (hint-tags))]
     [(string=? r "p") 
      (insert-into-xsel (current-passphrase)) (sleep 3) (insert-into-xsel "---") (mainloop #f)]
     [else ;treat as regular expression
      (let [(ms 
             (if with-tags
                 (let ([rr (split-on-first-comma r)])
                   (get-matches (cdr rr) (hints->tags with-tags (car rr))))
                 (get-matches r)))
            ]
        (if (cons? (hash-keys ms))
            (begin (suggest-matches ms)   (mainloop #f))
            (begin (nothing-found-info r) (mainloop #f))))])))

