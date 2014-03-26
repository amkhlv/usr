(provide 'amkhlv-tex)

(defun mytex/tex2svg (fontsize)
  "This first converts file to MathML, then from MathML to svg;
 file ---> file.mml ---> file.svg"
  (interactive "P")
  (save-buffer)
  (let* (
         (filepath (buffer-file-name))
         (jeuclid (if fontsize 
                      (concat "jeuclid-cli --fontSize " (int-to-string  fontsize))
                    "jeuclid-cli "
                    )
                  )
         )
    (shell-command (concat "blahtexml --mathml <\"" filepath "\" >\"" filepath ".mml\""))
    (shell-command (concat jeuclid " \""filepath ".mml\"  \""filepath ".svg\"")) 
    )
  )

(defun mytex/newletter ()
  "Open new latex letter. Will save in the current dir. The date _Y-m-d is attached to the filename. See also mytex/mailpdf"
  (interactive)
  (let* ((bn (read-from-minibuffer "basename: " ))
         (tstump (format-time-string "%Y-%m-%d"))
         (fn0 (concat bn "_" tstump))
         (existing (directory-files default-directory))
         (newname (let ((n 0)) (while (delq nil 
                                            (mapcar (lambda (x)
                                                      (string-match 
                                                       (if (> n 0) 
                                                           (concat 
                                                            (regexp-quote fn0) 
                                                            "_" (number-to-string n) "\\.tex$"
                                                            )
                                                         (regexp-quote fn0))
                                                       x)
                                                      ) existing)
                                            )
                                 (setq n (+ n 1))
                                 )
                       (if (> n 0) 
                           (concat fn0 "_" (number-to-string n))
                         fn0
                         )
                       )
                  )
         (snapshots-dir (concat "snapshots/" newname))
         )
    (if (file-exists-p "snapshots")
        (make-directory snapshots-dir)
      (progn
        (make-directory "snapshots")
        (make-directory snapshots-dir)
        )
      )
    (find-file (concat newname ".tex"))
    (my-org-shapka)
    (insert (concat "%%% filename:        " newname ".tex\n"))
    (insert (concat "%%% snapshots in:    " snapshots-dir "\n"))
    )
  )

(defun mytex/mailpdf ()
  "Given file.tex in the buffer, email (using mutt) file.pdf"
  (interactive)
  (let* ((texname (buffer-file-name))
         (pdfname (replace-regexp-in-string "\\.tex$" ".pdf" texname))
         )
    (start-process "mailpdf" "mailpdf" "urxvt" "-e" "mutt" "-a" pdfname)
    )
  )

(defun mytex/printpdf ()
  "Given file.tex in the buffer, print (asking for the printer) file.pdf"
  (interactive)
  (let* ((texname (buffer-file-name))
         (pdfname (replace-regexp-in-string "\\.tex$" ".pdf" texname))
         (pdfprinter (read-from-minibuffer "Enter the printer name: "))
         )
    (start-process "printpdf" "printpdf" "lpr" "-P" pdfprinter  pdfname)
    )
  )

(defun mytex/jumphref ()
  "Cursor should be on 'notes.pdf' in \href{notes.pdf#label}, 
   which should be all on one line; then jump to the corresponding tex file"
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward "\\href{\\(.*\\)\.pdf#\\([^}]*\\)}")
  (let ((tex-file-name (concat (match-string 1) ".tex"))
        (label-name (match-string 2))
        )
  (find-file tex-file-name)
  (beginning-of-buffer)
  (search-forward label-name)
  ))

(defun amkhlv/files/setup-lualatex ()
  (let ((pt (point)))
    (setq TeX-engine 'luatex)   
    (beginning-of-buffer)
    (message "here0")
    (if 
        (re-search-forward 
         "\\(-\\*-.*\\) andrei: .?.?.?LaTeX;\\(.*\\)\\(coding: *[^;[:space:]]+\\)\\(.*-\\*-\\)" 
         100 
         t )
        (progn 
          (message "here1")
          (replace-match "\\1 andrei: LuaLaTeX;\\2 coding: utf-8-unix\\4"))
      (if 
          (re-search-forward 
           "\\(-\\*-.*\\) andrei: .?.?.?LaTeX\\(.*-\\*-\\)" 
           100 
           t)
          (progn 
            (message "here2")
            (replace-match "\\1 andrei: LuaLaTeX; coding: utf-8-unix\\2"))
        )
      (insert "% -*- andrei: LuaLaTeX; coding: utf-8-unix -*-\n\n")
      )
    (goto-char pt)
    "% NOTE: C-c C-c bound to LuaLaTeX"
    ))
