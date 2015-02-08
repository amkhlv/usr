#!/usr/bin/env racket

#lang at-exp racket

(require (planet esilkensen/yaml) racket/system racket/list)

(define conf (call-with-input-file (string->path "REGISTRY.yaml") read-yaml))

(define (delete-png-formulas dir #:depth [depth 1])
  (when (directory-exists? dir)
    (for/list ([f
                (find-files 
                 (lambda (p)
                   (let ([ep (explode-path p)])
                     (and
                      ((length ep) . < . (+ 1 depth (length (explode-path dir))))
                      (regexp-match #px"^\\d+\\.png" (path->string (last ep))))))
                 dir)])
      (delete-file f))))
(define (delete-html-files dir #:depth [depth 1])
  (when (directory-exists? dir)
    (for/list ([f
                (find-files 
                 (lambda (p)
                   (let ([ep (explode-path p)])
                     (and
                      ((length ep) . < . (+ 1 depth (length (explode-path dir))))
                      (regexp-match #px".*\\.html" (path->string (last ep))))))
                 dir)])
      (delete-file f))))

(when (hash-has-key? conf "html")
  (for/list ([f (hash-ref conf "html")])
    (let ((f-html (string->path (string-append (hash-ref f "name") ".html")))
          (f-form (string->path (string-append (hash-ref f "name") "_formulas")))
          (f-sql  (string->path (string-append (hash-ref f "name") "_formulas.sqlite"))))
      (when (hash-has-key? f "dest")
        (delete-png-formulas (hash-ref f "dest"))
        (delete-html-files (hash-ref f "dest")))
      (when (file-exists? f-html) (delete-file f-html))
      (when (directory-exists? f-form) (delete-directory/files f-form))
      (when (file-exists? f-sql) (delete-file f-sql)))))
(when (hash-has-key? conf "htmls")
  (for/list ([f (hash-ref conf "htmls")])
    (let ((f-dir (string->path f))
          (f-form (string->path (string-append f "_formulas")))
          (f-sql (string->path (string-append f "_formulas.sqlite"))))
      (delete-png-formulas f-dir)
      (delete-html-files f-dir)
      (delete-png-formulas f-form)
      (when (file-exists? f-sql) (delete-file f-sql)))))
(delete-png-formulas (current-directory))
(let ((bp (string->path "bystrotex.fifo")))
  (when (file-exists? bp) (delete-file bp)))
