#!/usr/bin/env racket

#lang racket

(require (planet amkhlv/truques/xml))
(require racket/cmdline xml)

(define print-filename (make-parameter #f))


(define svg-filename
  (command-line 
   #:once-each
   ["--print-filename" "Print filename" (print-filename #t)]
   #:args (filename)
   filename
   ))
(define mainx (file->xexpr svg-filename))
(define qs 
  '((width  (svg #:width)) (height (svg #:height)))
  )

(when (print-filename) (display svg-filename) (display "\n"))

(define results
  (xexpr->xml
   (append
    '(results ())
    (for/list [(q qs)]
      `(,(car q)
        () ;no attributes
        ,(se-path* (cadr q) mainx))))))

(write-xml/content results)
(when (print-filename)  (display "\n"))


