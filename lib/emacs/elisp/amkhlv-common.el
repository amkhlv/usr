
(defun amkhlv-aux-replacer (regex-replacement-pairs mk1 mk2)
  (mapc 
   (lambda (x) 
     (while 
         (re-search-backward (car x) mk1 t)
       (replace-match (car (cdr x)) t)
       )
     (goto-char mk1)
     (set-mark-command nil)
     (goto-char mk2)
     )
   regex-replacement-pairs)
  )

(defmacro amkhlv-interactive-replacer (name regex-replacement-pairs)
"Example (amkhlv-interactive-replacer tex2scrbl '((\"\\\\\\\\end{equation}\" \"}|\") (\"\\\\\\\\end{itemize}\" \"}|\")))"
  `(defun ,name ()
     (interactive)
     (let* ((ml (mark-marker)) 
            (mr (point-marker))
            ) 
       (amkhlv-aux-replacer ,regex-replacement-pairs ml mr)
       )
     )
  )

(provide 'amkhlv-common)
        