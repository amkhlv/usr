(provide 'amkhlv)

(defun amkhlv/open-file-at-point () (interactive)
  "To open file at point with an external program"
  (let* (
         (tap (thing-at-point 'filename))
         (external-command (read-from-minibuffer "external command: "))
         )
    (start-process 
     external-command 
     external-command 
     external-command
     tap)))

(defun ainsert-date () (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun mypdf/jumplabel () (interactive) 
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
  "This is to import the image from camera using VLC"
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
  (let* (
         (fap (thing-at-point 'filename))
         (fname (if (string-match "^file:\/\/\\(.*\\)" fap)
                    (match-string 1 fap)
                  fap
                  )
                )
         )
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