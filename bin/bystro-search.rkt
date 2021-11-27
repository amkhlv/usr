#!/usr/bin/env racket

#lang racket

(require bystroTeX/xmlconf)

(define regular-expression
  (command-line
   #:program "search BystroTeX files in this folder for regexp"
   #:args (regular-expression) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   regular-expression))

(call-with-input-file
  "lookdown.lst"
  (lambda (lookdown.lst)
    (for ([ln (cons "." (sequence->list (in-lines lookdown.lst)))])
      (parameterize ([working-directory ln])
        (for ([nm (all-names)])
          (with-bystroconf
            (get-bystroconf nm)
            (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?)
            (when (call-with-input-file
                    (build-path ln name.scrbl)
                    (lambda (f) (for/or ([ln (in-lines f)]) (regexp-match (regexp regular-expression) ln))))
              (if multipage?
                  (displayln (path->string (build-path ln name "index.html")))
                  (displayln (path->string (build-path ln dest  name.html)))))))))))
  
