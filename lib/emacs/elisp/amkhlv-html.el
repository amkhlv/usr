(provide 'amkhlv-html)

(defun myhtml-insert-svg (vspace)
  "Insert the link to the svg file at point, into html; the filename in the text should have the .svg extension stripped"
  (interactive "P")
  (let* (
         (tap (thing-at-point 'filename))
         (svg-content 
          (with-temp-buffer
            (insert-file-contents (concat tap ".svg"))
            (buffer-string)))
         (svg-width  (progn 
                       (string-match " width=\"\\([0-9]*\\)\"" svg-content)
                       (let* ((wd-str (match-string 1 svg-content)) 
                              (wd-int (string-to-int wd-str))
                              (wd+-int (+ 1 wd-int)))
                         (int-to-string wd+-int))))
         (svg-height (progn 
                       (string-match " height=\"\\([0-9]*\\)\"" svg-content)
                       (let* ((ht-str (match-string 1 svg-content)) 
                              (ht-int (string-to-int ht-str))
                              (ht+-int (+ 1 ht-int)))
                         (int-to-string ht+-int))))
         (vspace-string (if vspace 
                            (concat " vspace=\"" (int-to-string vspace) "\" ")
                          " "))
         )
    (move-beginning-of-line nil)
    (insert "<object data=\"")
    (move-end-of-line nil)
    (insert ".svg\" width=\"" svg-width "\" height=\"" svg-height "\" " vspace-string " type=\"image/svg+xml\"> </object>")
    )
  )

(defun myhtml-uninsert-svg ()
  (interactive)
  (let* (
         (ln (progn
               (move-beginning-of-line nil)
               (kill-line)
               (car kill-ring)
               )
             )
         (fname (progn 
                  (string-match "data=\"\\(.*\\)\.svg\"" ln)
                  (match-string 1 ln)
                  )
                )
         )
    (insert fname)
    )
  )


(defun myhtml-insert-link ()
  (interactive)
  (let* ((linkaddress (read-from-minibuffer "link URI: "))
         (linktext    (read-from-minibuffer "caption: " )))
    (insert (concat "
<a href=\"" linkaddress "\" >" linktext "</a>

")  
            )
    )
  )

(defun myhtml-mathml ()
  "This is my parser of the html files with the LaTeX, which converts to the MathML;
 the new filename will be the same but .html replaced with .out.html"
  (interactive)
  (save-buffer)
  (let* ((filepath (buffer-file-name))
         (newpath  (progn
                     (string-match "\\(.*\\).html$" filepath)
                     (concat (match-string 1 filepath) ".out.html")
                     )
                   )
         )
    (shell-command (concat "mymathparse.pl \"" filepath "\" \"" newpath "\""))
    )
  )
