#!/usr/bin/env racket

#lang at-exp racket

(require shell/pipeline-macro)

(define-syntax (mkparser hs)
  (datum->syntax
   hs
   `(command-line
     #:usage-help ,(apply string-append (cdr (syntax->datum hs)))
     #:args (name)
     name)))
(define
  name
  @mkparser{
          _____________________________________________________________________________
            The argument is some name, e.g.: "TEXT".
            The files TEXT.scrbl and header_TEXT.tex and footer_TEXT.tex must be present.

            Sample header_TEXT.tex:

            \documentclass[11pt]{article}
              \usepackage{graphicx}
              \usepackage{hyperref}
              \usepackage{xcolor}

              \title{My Work}
              \begin{document}
              \maketitle 

            Sample footer_TEXT.tex:

              \end{document}
            _____________________________________________________________________________
            }
  )

(define scribble-file (string-append name ".scrbl"))
(define tex-output-file (string-append name ".tex"))
(define in-head (open-input-file #:mode 'text (string-append "header_" name ".tex")))
(define in-foot (open-input-file #:mode 'text (string-append "footer_" name ".tex")))

(define out (open-output-file #:mode 'text #:exists 'replace tex-output-file))

(define (do-head)
  (let ([ln (read-line in-head)])
    (unless
        (eof-object? ln)
      (displayln ln out)
      (when (regexp-match #rx"^%ABSTRACT" ln)
        (run-pipeline
         \|
         cat $scribble-file
         \|
         b2l --get-abstract
         \|>>
         displayln (string-trim _) out
         )
        )
      (do-head))))

(define (sanitize-line ln)
  (string-replace
   (string-replace
    ln
    "❨" "(\\!(")
   "❩" ")\\!)")
  )
   
(define (process-tex in)
  (let main
      ([skip? #f]
       [ln (read-line in)]
       )
    (if (eof-object? ln)
        (close-input-port in)
        (if skip?
            (let ([stop-skipping? (regexp-match #rx"^%BystroTeX-preamble-end" ln)])
              (main (not stop-skipping?) (read-line in)))
            (let ([start-skipping? (regexp-match #rx"^%BystroTeX-preamble-start" ln)])
              (unless start-skipping? (displayln (sanitize-line ln) out))
              (main start-skipping? (read-line in)))))))
            

(define (do-body)
  (run-pipeline
   \|
   cat $scribble-file
   \|
   b2l
   \|
   tail -n "+2"
   \|
   tr -d "〚〛"
   \|>
   process-tex))


(do-head)
(do-body)
(let foot ()
  (let ([ln (read-line in-foot)])
    (unless (eof-object? ln)
      (displayln ln out)
      (foot))))

(close-output-port out)
(close-input-port in-head)
