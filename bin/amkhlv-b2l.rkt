#!/usr/bin/env racket

#lang racket

(require shell/pipeline-macro racket/cmdline)

(define header (make-parameter (path->string (build-path (find-system-path 'home-dir) "a" "tech" "racket" "header.tex"))))
(define footer (make-parameter (path->string (build-path (find-system-path 'home-dir) "a" "tech" "racket" "footer.tex"))))

(define filename-without-extension
  (command-line
   #:once-each
   ["--header" h "header" (header h)]
   ["--footer" f "header" (header f)]
   #:args (name)
   name))

(define scribble-file (string-append filename-without-extension ".scrbl"))
(define tex-output-file (string-append filename-without-extension ".tex"))
(define in-head (open-input-file #:mode 'text (header)))
(define in-foot (open-input-file #:mode 'text (footer)))

(define out (open-output-file #:mode 'text #:exists 'replace tex-output-file))

(define (sanitize-line ln)
  (string-replace
   (string-replace
    (string-replace (string-replace ln "〚" "") "〛" "")
    "❨" "(\\!(")
   "❩" ")\\!)")
  )


(define (do-head)
  (let ([ln (read-line in-head)])
    (unless
        (eof-object? ln)
      (displayln ln out)
      (when (regexp-match #rx"^%ABSTRACT" ln)
        (run-pipeline
         \|
         cat $scribble-file
         =object-pipe=
         sanitize-line
         \|
         b2l --get-abstract
         \|>>
         displayln (string-trim _) out
         )
        )
      (do-head))))

   
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
   =object-pipe=
   sanitize-line
   \|
   b2l
   \|
   tail -n "+2"
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
