#lang at-exp racket

(define eqn-uri  (read-line (current-input-port) 'any))

(define eqn-elem-btw-paren (cadr (string-split eqn-uri "#")))

(define eqn1 (cadr (regexp-match #rx".*elem\\.\\_([^%].+)%.*" eqn-elem-btw-paren)))

(define (alphanum? c)
  (or (char-numeric? c)
      (char-alphabetic? c)))

(define eqn (list->string (filter alphanum? (string->list eqn1))))

;(with-output-to-file "/tmp/eqnum.txt" (display eqn))

(let-values ([(proc stdout stdin stderr) (subprocess #f #f (current-error-port) (find-executable-path "xsel") "-i" "-b")])
  (display eqn stdin)
  (close-output-port stdin)
  (close-input-port stdout)
  )

