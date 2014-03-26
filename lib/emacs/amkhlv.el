(provide 'amkhlv)

(require 'json)

(defun amkhlv/open-file-at-point () (interactive)
  "To open file at point with an external program"
  (let* (
         (tap (thing-at-point 'filename))
         (external-command (read-from-minibuffer "external command: "))
         )
    (start-process external-command external-command external-command tap)))

(defun ainsert-date () (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun amkhlv/jumplabel () (interactive) 
  (save-excursion 
    (let* ((bfn (buffer-file-name))
           (bfn-noext 
            (replace-regexp-in-string 
             ".*/" ""
             (replace-regexp-in-string "\.tex$" "" bfn)))
           (bfn-aux (replace-regexp-in-string "\.tex$" ".aux" bfn))
           (nearest-label (progn 
                            (re-search-backward "\\label{\\([^}]*\\)}")
                            (match-string 1)))
           (dest-in-pdf
            (with-temp-buffer
              (insert-file-contents bfn-aux)
              (beginning-of-buffer)
              (re-search-forward 
               (concat "\\newlabel{" 
                       nearest-label 
                       "}.*{\\([^}{]*\\)}{}}"))
              (match-string 1))))
      (message dest-in-pdf)
      (shell-command (concat "xpdf.real -remote " 
                             bfn-noext 
                             " -exec 'gotoDestNoScroll(" 
                             dest-in-pdf 
                             ")'")))))

(defun import-image (import-command)
  (cond
   ((eq major-mode 'latex-mode)
    (progn
      (shell-command import-command) 
      (move-beginning-of-line nil)
      (insert "\\hspace{-10pt}\n")
      (insert "\\includegraphics[scale=0.5]{")
      (move-end-of-line nil)
      (insert "}\\\\\n")))
   ((eq major-mode 'pod-mode)
    (progn 
      (shell-command import-command)
      (move-beginning-of-line nil)
      (insert "=for html <img src=\"")
      (move-end-of-line nil)
      (insert "\" align=\"center\" /><br>\n\n")))
   ((eq major-mode 'html-mode)
    (progn 
      (shell-command import-command)
      (move-beginning-of-line nil)
      (insert "<img src=\"")
      (move-end-of-line nil)
      (insert "\"/>")))
   ((eq major-mode 'scribble-mode)
    (progn 
      (shell-command import-command)
      (move-beginning-of-line nil)
      (insert "@(image (string->path \"")
      (move-end-of-line nil)
      (insert "\"))")))))

(defun amkhlv/mysh (noscale) (interactive "P")
  "This is to import the image, unscaled if used with a prefix; otherwise scaled"
  (let* ((tap (thing-at-point 'filename))
         (ext (progn (string-match "\\.\\([^.]*\\)$" tap) (match-string 1 tap)))
         (import-command
          (if noscale 
              (concat "import " tap " && mtpaint " tap)
            (concat "    import /tmp/tmp_mysh." ext 
                    " && mtpaint /tmp/tmp_mysh." ext 
                    " && convert /tmp/tmp_mysh." ext " -resize 760 -unsharp 10 -frame 2 "
                    (thing-at-point 'filename)))))
    (import-image import-command)))

(defun amkhlv/snapshot () (interactive)
  "This is to import image from android camera etc; the variable amkhlv/snapshot-dir should be set to where photos will appear"
  (let* ((tap (thing-at-point 'filename)))
    (if (boundp 'amkhlv/snapshot-dir)
        (progn
          (shell-command (concat "touch " amkhlv/snapshot-dir "/timeref"))
          (read-from-minibuffer "capture image and then press ENTER when done") 
          (import-image
           (concat "find " 
                   amkhlv/snapshot-dir 
                   " -type f -newer " 
                   amkhlv/snapshot-dir 
                   "/timeref -exec mv \\{\\} " 
                   tap 
                   " \\; ; mtpaint " 
                   tap)))
      (message "``I REFUSE: please set variable amkhlv/snapshot-dir to where the photos will appear''"))))


(defun amkhlv/vlc () (interactive)
  "This is to import the image from webcam using VLC"
  (let* (
         (tap (thing-at-point 'filename))
         (import-command 
          (concat
           "touch ~/aa/vlc-snapshots/timeref ;
            inotifywait ~/aa/vlc-snapshots/ ;
            find ~/aa/vlc-snapshots/ -type f -newer ~/aa/vlc-snapshots/timeref -exec mv \\{\\} " tap " \\; ;
            mtpaint " tap )))
    (import-image import-command)))

(defun amkhlv/bookmarks ()
  "Compiles bookmarks if in markdown-mode, otherwise loads bookmarks file"
  (interactive)
  (if (string= (format "%s" major-mode) "markdown-mode")
      (progn 
        (save-buffer)
        (shell-command "cd ~/a/homepage ; scribble bookmarks.scrbl"))
    (progn
      (find-file "~/a/bm.md")
      (hide-body))))

(defun amkhlv/ffap ()
  "This is the replacement of ffap to open correctly filenames like file://filename"
  (interactive)
  (let* ((fap (thing-at-point 'filename))
         (fname (if (string-match "^file:\/\/\\(.*\\)" fap)
                    (match-string 1 fap)
                  fap)))
    (find-file fname)))

(defun amkhlv/print/ () 
  "In emacs there are 2 types of print commands:
those starting from print- and those starting from ps-print-
The first type prints as plain text, the second as postscript.
First execute myprint/setup to choose the printer name
To choose size, use ps-print-customize"
  (interactive))

(defun amkhlv/print/setup ()
  "Sets up variables printer-name and ps-printer-name"
  (interactive)
  (setq printer-name (read-from-minibuffer "Enter the printer name: "))
  (setq ps-printer-name printer-name))

;; This below is for sqlite insertion:

(defvar alphabet 
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defun auxsqli (dbfile tbl) 
  (let* ((collist 
          (mapcar* 
           #'cons
           alphabet 
           (split-string 
            (shell-command-to-string 
             (concat 
              "sqlite3 " dbfile " 'pragma table_info(" tbl ")' | awk -F'|' '{print $2}'")))))
         (colhash (make-hash-table :test 'equal)))
    (mapc (lambda (x) (puthash  (car x)  (cdr x)  colhash)) collist) 
    (let* ((cols (read-from-minibuffer 
                  (concat 
                   "COLUMNS (press ENTER for *): " 
                   (apply #'concat (mapcar (lambda (x) (concat (car x) ":" (cdr x) "   ")) collist))
                   "=> ")))
           (colnames0 
            (delq nil (mapcar   (lambda (x) (gethash x colhash))    (split-string cols ""))))
           (colnames (if colnames0 (mapconcat 'identity colnames0 ",") "*")) 
           ;; if user did not select column (just press ENTER) default to all columns
           (sqlite3com
            (if colnames0 "sqlite3 -separator ' | ' " "sqlite3 -line ")) 
           ;; if did not select column (just press ENTER) default to all columns
           (query0 (concat "select " colnames " from " tbl " where "))
           (query (replace-regexp-in-string "%" "%%" (read-from-minibuffer "" query0))))
      (message query)
      (insert (shell-command-to-string (concat sqlite3com dbfile " \"" query "\"")))
      )))

(defun linii (dbfile tbl)
  "This function is for use inline, there is a corresponding yasnippet called linii"
  (let ((curpos (point-marker)))
    (backward-sexp)
    (kill-region (point-marker) curpos)
    (auxsqli dbfile tbl)
    )
  )

(defmacro mydef-sqli (name dbfile tbl)
  `(defun ,name ()
     (interactive)
     (auxsqli ,dbfile ,tbl)
     )
  )

(mydef-sqli amkhlv/linii-abk            "/home/andrei/a/tech/base/addr.db"       "abk") 
(mydef-sqli amkhlv/linii-myaddresses    "/home/andrei/alcl/tech/base/mylist.db"  "my_addresses")

(defun amkhlv/pdfview ()
  "View the PDF file corresponding to this TeX file using pdfviewer"
  (interactive)
  (make-local-variable 'pdfviewerPID)
  (make-local-variable 'pdfviewerProcess)
  (make-local-variable 'pdfFileName)
  (set 'pdfFileName (replace-regexp-in-string "\.tex$" ".pdf" (buffer-file-name)))
  (set 'pdfviewerProcess 
       (start-process 
        (concat "viewer-for-" (buffer-file-name))
        nil
        "/usr/local/bin/pdfviewer"
        pdfFileName))
  (set 'pdfviewerPID (process-id pdfviewerProcess)))

(defun amkhlv/pdfjump ()
  "Jump to the location in the pdf file"
  (interactive)
  (shell-command-to-string
   (concat 
    "qdbus glad.PdfViewer"
    (number-to-string pdfviewerPID)
    " /MainWindow glad.PdfViewer.syncFromSource "
    (buffer-file-name)
    " "
    (number-to-string (line-number-at-pos)))))
  
(defun amkhlv/yaml2json (filename)
   (shell-command-to-string 
    (concat 
     "python -c '"
     "import sys; import fileinput; import yaml; import json; "
     "fl = open(\"" 
     filename
     "\",\"r\"); "
     "x = yaml.safe_load(fl); "
     "print(json.dumps(x)); fl.close()"
     "'")))

(defun myscribble/compile ()
  "Runs scribble on the file"
  (interactive)
  (save-buffer)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (registry
          (if (file-exists-p "REGISTRY.yaml")
              (json-read-from-string (amkhlv/yaml2json "REGISTRY.yaml"))
            '())))
    (if (car registry)
        (progn 
          (message (concat "looking up " filename " in registry"))
                                        ; First we iterate over --html :
          (dolist
              (h (mapcar (lambda (x) x) ; this is to convert vector to list
                         (cdr (assoc 'html registry))))
            (message (concat "considering " (cdr (assoc 'name h))))
            (when (string= filename (concat (cdr (assoc 'name h)) ".scrbl"))
              (if (cdr (assoc 'dest h))
                  (progn
                    (message (concat "scribble single html " filename " ⇨ " (cdr (assoc 'dest h)) "/"))
                    (shell-command (concat "scribble --dest " (cdr (assoc 'dest h)) " " filename)))
                (progn
                  (message (concat "scribble " filename))
                  (shell-command (concat "scribble " filename))))))
                                        ; Then iterate over --htmls :
          (dolist
              (h (mapcar (lambda (x) x) ; this is to convert vector to list
                         (cdr (assoc 'htmls registry))))
            (when (string= filename (concat h ".scrbl")) 
              (message (concat "scribble  multipage " filename))
              (shell-command (concat "scribble --htmls " filename)))))
      (progn
        (message "no registry found, proceeding with single html")
        (shell-command (concat "scribble " filename))))))

(defun myscribble/compile-htmls ()
  "Runs scribble -htmls on the file"
  (interactive)
  (save-buffer)
  (let* ((filepath (buffer-file-name)))
    (shell-command (concat "scribble --htmls " filepath))
    )
  )

(defun myscribble/view ()
  "This is mapped to C-c C-v in the scribble mode;
 looks up the corresponding .html file and runs firefox on it"
  (interactive)
  (let* ((filepath (buffer-file-name))
         (filename (file-name-nondirectory filepath))
         (htmlpath (progn
                     (string-match "\\(.*\\)\.scrbl$" filepath)
                     (match-string 1 filepath)))
         (registry
          (if (file-exists-p "REGISTRY.yaml")
              (json-read-from-string (amkhlv/yaml2json "REGISTRY.yaml"))
            '())))
    (if (cdr registry)
        (progn 
          (message (concat "looking up " filename " in registry"))
                                        ; First we iterate over --html :
          (dolist
              (h (mapcar (lambda (x) x) ; this is to convert vector to list
                         (cdr (assoc 'html registry))))
            (message (concat "considering " (cdr (assoc 'name h))))
            (when (string= filename (concat (cdr (assoc 'name h)) ".scrbl"))
              (if (cdr (assoc 'dest h))
                  (progn
                    (message (concat "viewing single html " 
                                     (cdr (assoc 'dest h)) 
                                     "/" 
                                     (cdr (assoc 'name h)) 
                                     ".html"))
                    (shell-command (concat "firefox " 
                                           (cdr (assoc 'dest h)) 
                                           "/" 
                                           (cdr (assoc 'name h))
                                           ".html"
                                           )))
                (progn
                  (message (concat "viewing single html " (cdr (assoc 'name h)) ".html"))
                  (shell-command (concat "firefox " (cdr (assoc 'name h)) ".html"))))))
                                        ; Then iterate over --htmls :
          (dolist
              (h (mapcar (lambda (x) x) ; this is to convert vector to list
                         (cdr (assoc 'htmls registry))))
            (when (string= filename (concat h ".scrbl")) 
              (message (concat "viewing multipage, starting at " h "/index.html"))
              (shell-command (concat "firefox " h "/index.html")))))
    (shell-command (concat "firefox "  htmlpath ".html")))))
