#lang scribble/base
@(require scribble/core scribble/html-properties scriblib/render-cond)
@(require bystroTeX/common (prefix-in truques: truques/truques))


@(require scribble/decode)
@(require racket)

@(define (pagename) 
   (let* [(curdir  (path->string (current-directory)))
          (homedir (getenv "HOME"))
          (n (string-length homedir))
          ]
     (if (string=? (substring curdir 0 n) homedir)
         (substring curdir (+ 1 n) (- (string-length curdir) 1))
         curdir)
         ))

@(define (basename p)
   (let-values ([(base name mustbedir) (split-path p)]) 
     name))

@(define (fl-lnk p f)
   (hyperlink (bystro-path-to-link (build-path p f)) (path->string f)))

@(define (scrbl-html-lnk p f)
   (let* ([s (path->string f)]
          [n (string-length s)]
          [h (string-append (substring s 0 (- n 6)) ".html")])
     (elem " ❄ "(hyperlink (bystro-path-to-link (string-append (path->string p) "/" h)) (substring s 0 (- n 6))))))

@(define (list-dirs p)
   (let ([fs (directory-list p)])
      (filter (curry bystro-dir-contains-scrbl? #:exclude-same-name #f) (map (lambda (u) (build-path p u)) fs))))

@(define (top-dir-decl p)
   (title-decl #f '() #f (make-style #f '()) (hyperlink (bystro-path-to-link p) (path->string (basename p)))))

@title[#:style '(no-toc no-sidebar)]{@pagename[]}
@(truques:curdir)

@(display (current-directory))

@(map (lambda (u) (scrbl-html-lnk (current-directory) u)) (bystro-list-scrbls #:exclude-same-name #f (current-directory)))

@(define
   (top-dir-list p)
   (decode 
    (append 
     (list (top-dir-decl p)) 
     (map (lambda (u) (scrbl-html-lnk p u)) (bystro-list-scrbls #:exclude-same-name #f p))
     (apply append (map 
                    (lambda (u) (dir-list u 0)) 
                    (list-dirs p))))))

@(define
   (dir-list p i)
   (append 
    (list (part-start i #f '() (make-style #f '()) 
                      (hyperlink (bystro-path-to-link p) (path->string (basename p)))))
    (map 
     (lambda (u) (scrbl-html-lnk p u)) 
     (bystro-list-scrbls #:exclude-same-name #f p))             
    (apply 
     append 
     (map 
      (lambda (u) (dir-list u (+ 1 i))) 
      (list-dirs p)))))
           
@(map top-dir-list (list-dirs (current-directory)))


