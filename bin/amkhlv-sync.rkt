#!/usr/bin/env racket

#lang racket

(require (for-syntax racket/syntax))
(require racket/cmdline racket/string) 
(require (planet amkhlv/bystroTeX/utils) (planet amkhlv/truques/terminal))

(define testing #f)

(define LIST_OF_FILES_TO_SYNC "files-to-sync.txt")

;;on USB drive:
(define USB_MOUNT (build-path (find-system-path 'home-dir) "pny8"))
(define MAIN_ON_USB (build-path USB_MOUNT "Andrei"))
(define TMP_ON_USB (build-path USB_MOUNT "tmp"))

;;on computer:
(define HOME_DIR (find-system-path 'home-dir))
(define A_DIR (build-path HOME_DIR "a"))
(define SYNCED_HOME_IN_A (build-path A_DIR "other" "home"))
(define AA_DIR (build-path HOME_DIR "aa"))

;;for testing:
(when testing
  (set! USB_MOUNT (build-path (find-system-path 'home-dir) "a" "fakeusb"))
  (set! MAIN_ON_USB (build-path USB_MOUNT "Andrei"))
  (set! TMP_ON_USB (build-path USB_MOUNT "tmp"))
  (set! A_DIR (build-path (find-system-path 'home-dir) "a" "fakea"))
  (set! SYNCED_HOME_IN_A (build-path A_DIR "other" "home"))
  (set! AA_DIR (build-path (find-system-path 'home-dir) "a" "fakeaa"))
  (set! HOME_DIR (build-path (find-system-path 'home-dir) "a" "fakehome"))
  )


;;------------------------------------------------------------------------------------------------------
;; Do not modify below this line

(define
  usb-mount-dir (string-append (path->string USB_MOUNT) "/"))
(define
  usb-main-dir (string-append (path->string MAIN_ON_USB) "/"))
(define
  usb-tmp-dir (string-append (path->string TMP_ON_USB) "/"))
(define
  list-of-files-to-sync-in-a (path->string (build-path SYNCED_HOME_IN_A LIST_OF_FILES_TO_SYNC)))
(define
  list-of-files-to-sync-in-home (path->string 
                                 (build-path HOME_DIR LIST_OF_FILES_TO_SYNC)))
(define ;this is *only* for syncing HOME files
  computer-home-dir (string-append (path->string HOME_DIR) "/"))
(define
  computer-a-dir (string-append (path->string A_DIR) "/"))
(define
  computer-synced-home-dir-in-a (string-append (path->string SYNCED_HOME_IN_A) "/"))
(define
  computer-aa-dir (string-append (path->string AA_DIR) "/"))

;; various definitions
(define (high-yellow x) (ansi-bg256 228 (ansi-fg256 0 x)))
(define (strong-warning x) (ansi-bold (ansi-bg256 1 (ansi-fg256 15 x))))
(define (broken-link-warning x) (ansi-fg256 166 x))

;; command line parsing
(define direction=in? (make-parameter #f))
(define direction=out? (make-parameter #f))
(define fast-mode? (make-parameter #f))
(define dry-run?   (make-parameter #f))
(command-line 
 #:once-each
 ["-f" ("Fast") (fast-mode? #t)]
 ["-n" ("Dry run") (dry-run? #t)]
 #:once-any
 ["-o" ("Take out") (direction=out? #t)]
 ["-i" ("Take in")  (direction=in? #t)])

(define rsync_short-flags (if (dry-run?) "-an" (if (fast-mode?) "-a" "-ca")))

;; some global paremeters
(define current-md5s-in-a (make-parameter '()))
(define current-md5s-in-aa (make-parameter '()))

;; main definitions ------------------------------------------------------------------------------------

(define (display-bold . x) 
  (display (ansi-bold (string-join x)))
  (display "\n"))

(define (show-errors inport)
  (for ([l (in-lines inport)])
    (display (ansi-reverse l))
    (display "\n")))

(define (show-lines inport (prefix ""))
  (for ([l (in-lines inport)])
    (display prefix)
    (display l)
    (display "\n")))

(define (process-rsync-output inport from-dir to-dir current-md5-list)
  (for ([l (in-lines inport)])
    (let* ([itmzd (substring l 0 11)]
           [name  (substring l 12)]
           [received? (equal? (substring itmzd 0 1) ">")]
           [changed?  (equal? (substring itmzd 0 1) "c")]
           [message?  (equal? (substring itmzd 0 1) "*")]
           [type (substring itmzd 1 2)]
           [file? (equal? type "f")]
           [dir?  (equal? type "d")]
           [symlink? (equal? type "L")]
           [device?  (equal? type "D")]
           [special? (equal? type "S")]
           )
      (define info-string
        (apply ansi-fg256
               (cond
                [file?    '(46  "┃ ")]
                [dir?     '(21  "┃ ")]
                [symlink? '(39  "┣━")]
                [device?  '(160 "╋╋")]
                [special? '(226 "┃┃")])))
      (unless (or message? (and changed? dir?)) 
        (displayln (string-join (list info-string 
                                      from-dir 
                                      (if symlink? (ansi-fg256 39 "┬─") info-string)
                                      name)))
        (when symlink? 
          (let ([spaces (string-join (list (ansi-fg256 39 "┃ ") 
                                           (make-string (string-length from-dir) #\space)))])
            (with-external-command-as 
             r 
             ("realpath" (string-append from-dir name))
             (let ([x (read-line r-stdout)]) 
               (if (eof-object? x) 
                   (with-external-command-as
                    ls
                    ("ls" "-lh" (string-append from-dir name))
                    (let ([y (read-line ls-stdout)])
                      (if (eof-object? y)
                          (displayln (strong-warning (string-join `("ERROR:" ,name))))
                          (displayln (ansi-fg256 166 y)))))
                   (displayln (string-join (list spaces (ansi-fg256 39 "╰─") x)))))))))
      (when (or received? changed?)
        (when file?
          (if (file-exists? (string-append from-dir name))
              (with-external-command-as
               md5-calc
               #:cmdline
               `("md5sum" ,(string-append from-dir name))
               (current-md5-list 
                (cons 
                 (string-join 
                  `(,(car (string-split (read-line md5-calc-stdout)))
                    ,(string-append to-dir name)))
                 (current-md5-list)))
               (show-errors md5-calc-stderr)
               (md5-calc-ctrl 'wait))
              (displayln (string-join `(,(ansi-bg256 16 (ansi-fg256 160 "✕✕✕")) ,l))))))
      (when message?
        (displayln (ansi-bg256 183 (ansi-fg256 16 l)))))))

(define-syntax (sync-files:a->home stx)
  (let* ([command-with-args '("rsync" 
                              "-r" 
                              rsync_short-flags 
                              "--delete"
                              "--files-from" 
                              list-of-files-to-sync-in-a 
                              computer-synced-home-dir-in-a
                              computer-home-dir)])
    #`(begin
        (display-bold #,@command-with-args)
        (with-external-command-as
         fsyncer
         (#,@command-with-args)
         (show-lines fsyncer-stdout)
         (show-errors fsyncer-stderr)
         (fsyncer-ctrl 'wait)
         ))))

(define-syntax (sync-files:home->a stx)
  (let* ([command-with-args '("rsync" 
                              "-r" 
                              rsync_short-flags 
                              "--delete"
                              "--files-from" 
                              list-of-files-to-sync-in-home
                              computer-home-dir
                              computer-synced-home-dir-in-a)])
    #`(begin
        (display-bold #,@command-with-args)
        (with-external-command-as
         fsyncer
         (#,@command-with-args)
         (show-lines fsyncer-stdout)
         (show-errors fsyncer-stderr)
         (fsyncer-ctrl 'wait)
         ))))

(define-syntax (sync-a-and-aa:computer->usb stx)
  (syntax-case stx ()
    [(_ flag ...)
     (let* ([args-a '(rsync_short-flags computer-a-dir usb-main-dir)]
            [args-aa '(rsync_short-flags computer-aa-dir usb-tmp-dir)])
       #`(begin
           (display-bold "rsync" flag ... #,@args-a)
           (with-external-command-as
            fsyncer
            ("rsync" flag ... #,@args-a)
            (process-rsync-output fsyncer-stdout computer-a-dir usb-main-dir current-md5s-in-a)
            (show-errors fsyncer-stderr)
            (fsyncer-ctrl 'wait))
           (display-bold "rsync" flag ... #,@args-aa)
           (with-external-command-as
            fsyncer
            ("rsync" flag ... #,@args-aa)
            (process-rsync-output fsyncer-stdout computer-aa-dir usb-tmp-dir current-md5s-in-aa)
            (show-errors fsyncer-stderr)
            (fsyncer-ctrl 'wait))
           ))]))

(define-syntax (sync-a-and-aa:usb->computer stx)
  (syntax-case stx ()
    [(_ flag ...)
     (let* ([args-a '(rsync_short-flags usb-main-dir computer-a-dir)]
            [args-aa '(rsync_short-flags usb-tmp-dir computer-aa-dir)])
       #`(begin
           (display-bold "rsync" flag ... #,@args-a)
           (with-external-command-as
            fsyncer
            ("rsync" flag ... #,@args-a)
            (show-errors fsyncer-stderr)
            (fsyncer-ctrl 'wait))
           (display-bold "rsync" flag ... #,@args-aa)
           (with-external-command-as
            fsyncer
            ("rsync" flag ... #,@args-aa)
            (show-errors fsyncer-stderr)
            (fsyncer-ctrl 'wait))
           ))]))

(define-syntax (check-md5s stx)
  (let* ([command-with-args '("md5sum" "--strict" "--quiet" "-c" "-")])
    (syntax-case stx ()
      [(_ md5-list)
       #`(begin
           (display-bold #,@command-with-args)
           (with-external-command-as
            md5-checker
            (#,@command-with-args)
            (for ([x md5-list]) (display (string-append x "\n") md5-checker-stdin))
            (close-output-port md5-checker-stdin)
            (for ([l (in-lines md5-checker-stdout)]) (display l) (display "\n"))))])))

;; main ------------------------------------------------------------------------------------------------

(unless (fast-mode?) 
  (display "Are you sure you want SLOW sync?\n")
  (let ([a (read-line)])
    (unless (equal? a "y") (display "Then use the -f flag\n") (exit))))

(when (direction=out?) 
  (sync-files:home->a)
  (sync-a-and-aa:computer->usb "--delete" "--out-format=%i %n")
  (high-yellow "syncing...")
  (with-external-command-as sync #:cmdline '("sync") (sync-ctrl 'wait))
  (high-yellow "checking a:\n")
  (check-md5s (current-md5s-in-a))
  (high-yellow "checking aa:\n")
  (check-md5s (current-md5s-in-aa))
  )

(when (direction=in?)
  (display "SYNC IN?\n")
  (let ([a (read-line)])
    (unless (equal? a "YES") (display "you would have typed YES\n") (exit)))
  (sync-a-and-aa:usb->computer "--delete" "--out-format=%i %n")
  (sync-files:a->home)
  )
