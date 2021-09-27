#!/usr/bin/env racket

#lang at-exp racket

(require bystroTeX/utils dirname)
(define templates-dir (build-path (find-system-path 'home-dir) ".config" "inkscape" "templates"))

(define newsvg (make-parameter #f))
(define outpdf (make-parameter #f))
(define outdir (make-parameter #f))

(define-syntax (mkparser hs)
  (datum->syntax
   hs
   `(command-line
     #:program "SVG Staff"
     #:usage-help ,(apply string-append (cdr (syntax->datum hs)))
     #:once-each
     [("-n" "--new")
      n
      "new --- the argument should be the name of new file WITHOUT .svg"
      (newsvg n)]
     [("--to-pdf")
      opdf
      "staple to single PDF file (provide SVGs as arguments)"
      (outpdf opdf)]
     [("--to-svg")
      svgdir
      "burst PDF to SVGs (provide input PDF file as a single argument)"
      (outdir svgdir)]
     #:args fs
     fs
     )))

(define input-files
  @mkparser{
          _____________________________________________________________________________
            SVG staff
            _____________________________________________________________________________
            }
  )

(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (define (stty x) (system (~a "stty " x)) (void))
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(when (newsvg)
  (define
    templates
    (for/list
        [(i (in-naturals))
         (j (directory-list templates-dir))
         ]
      (cons (string-ref (number->string i) 0) j)))
  (for ([template templates])
    (display (car template))
    (display " ")
    (displayln (path->string (cdr template))))
  (with-raw
    (let* ([c (read-char)]
           [template (cdr (car (filter (λ (x) (eq? c (car x))) templates)))])
      (with-subprocess-as
        starlet
        (open-output-file (string-append (newsvg) ".svg"))
        #f
        (current-error-port)
        ("xmlstarlet"
         "ed"
         "-N"
         "svgns='http://www.w3.org/2000/svg'"
         "-u"
         "'//svgns:svg/@sodipodi:docname'"
         "-v"
         (newsvg)
         (path->string (build-path templates-dir template))
         )
        )
      )
    )
  )

(when (outpdf)
  (for ([svg-file input-files])
    (define-values (sp x y z)
      (subprocess
       (current-output-port)
       #f
       (current-error-port)
       (find-executable-path "inkscape")
       "-o"
       (string-append svg-file ".pdf")
       svg-file))
    (subprocess-wait sp)
    )

  (define-values (sp x y z)
    (apply
     subprocess
     `(,(current-output-port)
       #f
       ,(current-error-port)
       ,(find-executable-path "pdftk")
       ,@(map (λ (x) (string-append x ".pdf")) input-files)
       "cat"
       "output"
       ,(outpdf))))
  (subprocess-wait sp)
  )

(when (outdir)
  (subprocess
   (current-output-port)
   #f
   (current-error-port)
   (find-executable-path "pdf2svg")
   (car input-files)
   (path->string (build-path (outdir) (string-replace (basename (car input-files)) #rx"\\.pdf$" "_%03d.svg")))
   "all")
  )
