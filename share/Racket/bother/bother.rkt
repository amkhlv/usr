#lang racket

(require json xml xml/path racket/unix-socket bystroTeX/utils truques/xml)

(define td-config-file (build-path (find-system-path 'home-dir) ".config" "amkhlv" "td.hs.xml"))
(define qml-file (build-path (find-system-path 'home-dir) ".config" "amkhlv" "QML" "bother.qml"))
(define socket-path (build-path (find-system-path 'home-dir) ".local" "var" "run" "bother.sock"))

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

(define (initialize iostdin)
  (write-json (hasheq 'init  "init") iostdin)
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
    #:cmdline `("ioqml" "--name" "bother" "--class" "Bother" ,(path->string qml-file))
    (initialize ioqml-stdin)
    (refresh ioqml-stdin)
    (define (wait-on-pipe)
      (match (with-input-from-string (read-line ioqml-stdout) read-json)
        [(hash-table ('status "ready"))
         ;; not used
         (wait-on-pipe)]
        [(hash-table ('add x))  (with-todolist-file add-item x) (refresh ioqml-stdin) (wait-on-pipe)]
        [(hash-table ('del x))  (with-todolist-file del-item x)
                                (refresh ioqml-stdin)
                                (wait-on-pipe)] 
        [(hash-table ('repl (list orig-item new-item))) (with-todolist-file mod-item orig-item new-item)
                                                        (refresh ioqml-stdin)
                                                        (wait-on-pipe)]
        [x (thread-send main x)]
        ))
    (wait-on-pipe)
    )
  )

(define running-timer #f)
(define (set-timer n)
  (set! running-timer
        (thread
         (lambda ()
           (sleep (* n 60))
           (set! running-timer #f)
           (thread-send main 'show-gui)))))

(define (socket-listener from-socket to-socket)
  (thread
   (lambda ()
     (let wait-on-socket ()
       (match (read-line from-socket)
         ["show"
          (when running-timer (kill-thread running-timer) (set! running-timer #f))
          (thread-send main 'show-request-on-socket)
          (wait-on-socket)
          ]
         ["exit"
          (thread-send main 'exit-request-on-socket)
          ]
         [x
          (if (eof-object? x)
              (let () (display "bother: \"disconnect\"\n") (thread-send main 'socket-needs-to-be-restarted))
              (let ()
                (display (string-append "bother: \"unknown command on socket: >>>" x "<<<\"\n"))
                (wait-on-socket)))
          ]
         )))))
(define (start-socket-listener)
  (thread
   (lambda ()
     (let restart-loop ()
       (display (string-append "bother: control socket: " (path->string socket-path) "\n"))
       (let-values ([(from-socket to-socket) (unix-socket-accept (unix-socket-listen socket-path))])
         (display (string-append "bother: \"socket server listening on \"" (path->string socket-path) "\"\n"))
         (define listener-thread (socket-listener from-socket to-socket))
         (thread-send main 'socket-ok)
         (let repeat ()
           (match (thread-receive)
             ['exit
              (display "bother: \"closing socket server...\"\n")
              (kill-thread listener-thread)
              (close-input-port from-socket)
              (close-output-port to-socket)
              ]
             ['restart
              (display "bother: \"restarting socket server...\"\n")
              (kill-thread listener-thread)
              (close-input-port from-socket)
              (close-output-port to-socket)
              (when (file-exists? socket-path) (delete-file socket-path))
              (restart-loop)
              ]
             )))))))

;; ============================== MAIN ============================================================
(define main
  (thread
   (lambda ()
     (let ([sock-thread (start-socket-listener)])
       (let wait-for-mail ([socket-ok? #f])
         (match (thread-receive)
           ['socket-ok (wait-for-mail #t)]
           ['show-gui  (bother) (wait-for-mail socket-ok?)]
           [(hash-table ('snooze n))  (set-timer n) (wait-for-mail socket-ok?)]
           ['show-request-on-socket  (bother) (wait-for-mail #t)]
           ['exit-request-on-socket
            (display "bother: \"exiting on request from socket\"\n")
            (thread-send sock-thread 'exit)
            (when running-timer (kill-thread running-timer) (set! running-timer #f))
            #t]
           ['socket-needs-to-be-restarted
            (thread-send sock-thread 'restart)
            (wait-for-mail #f)
            ]
           [(hash-table ('command cmd))
            (match cmd
              ["exit"
               (if socket-ok?
                   (thread-send sock-thread 'exit)
                   (kill-thread sock-thread))
               #t]
              )]
           ))))))
(thread-send main 'show-gui)
(thread-wait main)
(when (file-exists? socket-path) (delete-file socket-path))
