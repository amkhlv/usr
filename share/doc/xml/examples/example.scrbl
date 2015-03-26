#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (planet amkhlv/truques/xml))

@title[#:style '(no-toc no-sidebar)]{Example}

@(let ([x (file->xexpr  "example.xml")])
   (itemlist
    (for/list ([p (se-path*/list '(catalog) x)] #:when (cons? p))
      (item (apply string-append (se-path*/list '(author) p))))))



 
  
