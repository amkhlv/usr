#!/usr/bin/env racket

#lang at-exp racket

(require (planet esilkensen/yaml) racket/system)
(require racket/cmdline)



(define conf (call-with-input-file (string->path "REGISTRY.yaml") read-yaml))

(command-line 
 #:args fids
 (when (hash-has-key? conf "html")
   (for/list ([f (hash-ref conf "html")])
     (when (or (null? fids) (member f fids))
       (if (hash-has-key? f "dest")
           (begin
             (system (string-append "scribble ++arg --dest --dest " (hash-ref f "dest") " " (hash-ref f "name") ".scrbl"))
             (system (string-append "ln -s " (hash-ref f "dest") "/" (hash-ref f "name") ".html ./")))
           (system (string-append "scribble  " (hash-ref f "name") ".scrbl"))))))
 (when (hash-has-key? conf "htmls")
   (for/list ([f (hash-ref conf "htmls")])
     (when (or (null? fids) (member f fids))
       (system (string-append "scribble ++arg --htmls --htmls " f ".scrbl"))))))
