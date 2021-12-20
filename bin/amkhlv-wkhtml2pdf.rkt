#!/usr/bin/env racket

#lang racket

(require racket/cmdline)

(define-for-syntax default-bottom-margin "30mm")
(define-for-syntax default-top-margin "30mm")
(define-for-syntax default-left-margin "20mm")
(define-for-syntax default-right-margin "20mm")
(define-for-syntax default-output-file-name #f)

(define margin-bottom (make-parameter #f))
(define margin-top (make-parameter #f))
(define margin-left (make-parameter #f))
(define margin-right (make-parameter #f))
(define output-file-name (make-parameter #f))

(define-syntax (mkparser self)
  (datum->syntax
   self
   `(begin
      (define htmls
        (command-line
         #:program "html printer"
         #:once-each
         ["--margin-bottom" m ,(format "bottom margin, default: ~s" default-bottom-margin) (margin-bottom m)]
         ["--margin-top" m ,(format "top margin, default: ~s" default-top-margin) (margin-top m)]
         ["--margin-left" m ,(format "left margin, default: ~s" default-left-margin) (margin-left m)]
         ["--margin-right" m ,(format "right margin, default: ~s" default-right-margin) (margin-right m)]
         ["--output" m ,(format "output-file-name file, default: ~s" default-output-file-name) (output-file-name m)]
         #:args x
         x)
        )
      (margin-bottom ,default-bottom-margin)
      (margin-top  ,default-top-margin)
      (margin-left ,default-left-margin)
      (margin-right ,default-right-margin)
      )))

(mkparser)

(for ([html htmls])
  (let-values
      ([(proc a b c) 
        (subprocess
         (current-output-port)
         #f
         (current-error-port)
         (find-executable-path "wkhtmltopdf")
         "--margin-bottom"
         (margin-bottom)
         "--margin-top"
         (margin-top)
         "--margin-left"
         (margin-left)
         "--margin-right"
         (margin-right)
         "--enable-local-file-access"
         html
         ((output-file-name) . or . (string-append html ".pdf"))
         )])
    (subprocess-wait proc)
    )
  )


