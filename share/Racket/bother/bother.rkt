#lang racket

(require json xml xml/path bystroTeX/utils truques/xml)

(define td-config-file (build-path (find-system-path 'home-dir) ".config" "amkhlv" "td.hs.xml"))
(define qml-file (build-path (find-system-path 'home-dir) ".config" "amkhlv" "QML" "bother.qml"))

;;--------------------------------------------------------------------------------------------------

(define todolist-file
  (let ([x (file->xexpr td-config-file)])
    (car
     (for/list ([tdlst (se-path*/list '(todolists) x)]
                #:when (and (cons? tdlst) (equal? "bother" (se-path* '(todolist #:name) tdlst))))
       (se-path* '(path) tdlst)))))

(define (xexpr->json x)
  (for/list ([todo (se-path*/list '(todolist) x)] #:when (cons? todo))
    (hasheq 'nick (se-path* '(todo #:nick) todo) 'urgent (cons? (member '(urgent ()) (se-path*/list '(todo) todo))))))

(define (refresh iostdin)
  (write-json (hasheq 'todolist  (xexpr->json (file->xexpr todolist-file))) iostdin)
  (display "\n" iostdin)
  (flush-output iostdin))

(define-syntax-rule (with-todolist-file cmd ...)
  (let* ([modified-list (xexpr->xml (cmd ...))]
         [port (open-output-file todolist-file #:exists 'replace)]
         )
    (write-xml/content modified-list port)
    (close-output-port port)))

(define (urgent? x) (equal? #\! (string-ref x (- (string-length x) 1))))
(define (w/out-exclamation x) (if (urgent? x) (substring x 0 (- (string-length x) 1)) x))

(define (add-item x)
  (let ([original (file->xexpr todolist-file)])
    (cons
     'todolist
     (cons
      '()
      (cons
       `(todo ((nick ,(w/out-exclamation x))) ,@(if (urgent? x) '((urgent ())) '()))
       (se-path*/list '(todolist) original))))))
(define (del-item x)
  (let ([original (file->xexpr todolist-file)])
    (cons
     'todolist
     (cons
      '()
      (for/list
          ([td (se-path*/list '(todolist) original)] #:when (cons? td) #:unless (equal? x (se-path* '(todo #:nick) td)))
        td)))))
(define (mod-item orig-item new-item)
  (let ([original (file->xexpr todolist-file)])
    (cons
     'todolist
     (cons
      '()
      (for/list
          ([td (se-path*/list '(todolist) original)] #:when (cons? td))
        (if (equal? orig-item (se-path* '(todo #:nick) td))
            `(todo ((nick ,(w/out-exclamation new-item))) ,@(if (urgent? new-item) '((urgent ())) '()))
            td))))))

(define (bother)
  (with-external-command-as ioqml
    #:cmdline `("ioqml" ,(path->string qml-file))
    (refresh ioqml-stdin)
    (define (wait-on-pipe)
      (match (with-input-from-string (read-line ioqml-stdout) read-json)
        [(hash-table ('snooze n))
         (sleep (* 60 n))
         (bother)]
        [(hash-table ('command cmd))
         (match cmd
           ["exit" #t]
           )]
        [(hash-table ('add x))
         (with-todolist-file add-item x)
         (refresh ioqml-stdin)
         (wait-on-pipe)]
        [(hash-table ('del x))
         (with-todolist-file del-item x)
         (refresh ioqml-stdin)
         (wait-on-pipe)] 
        [(hash-table ('repl (list orig-item new-item)))
         (with-todolist-file mod-item orig-item new-item)
         (refresh ioqml-stdin)
         (wait-on-pipe)]
        ))
    (wait-on-pipe)
    )
  )

(bother)
