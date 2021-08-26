#!/usr/bin/env racket

#lang racket

(require racket/cmdline)

(define htmls
  (command-line
   #:program "html printer"
   #:args x
   x)
  )

(for ([html htmls])
  (subprocess
   (current-output-port)
   #f
   (current-error-port)
   (find-executable-path "wkhtmltopdf")
   "--margin-bottom"
   "30mm"
   "--margin-top"
   "30mm"
   "--margin-left"
   "20mm"
   "--margin-right"
   "20mm"
   "--enable-local-file-access"
   html
   (string-append html ".pdf")
   )
  )


