#!/usr/bin/env racket

#lang racket

(require racket/cmdline)

(define-for-syntax default-bottom-margin "30mm")
(define-for-syntax default-top-margin "30mm")
(define-for-syntax default-left-margin "20mm")
(define-for-syntax default-right-margin "20mm")


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
         #:args x
         x)
        )
      (define margin-bottom (make-parameter ,default-bottom-margin))
      (define margin-top (make-parameter ,default-top-margin))
      (define margin-left (make-parameter ,default-left-margin))
      (define margin-right (make-parameter ,default-right-margin))
      )))

(mkparser)

(for ([html htmls])
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
   (string-append html ".pdf")
   )
  )


