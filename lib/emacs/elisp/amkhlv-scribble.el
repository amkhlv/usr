(provide 'amkhlv-scribble)

(defun amkhlv/scribble/compile ()
  "Runs scribble on the file"
  (interactive)
  (save-buffer)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (exists-xml? (file-exists-p "bystrotex.xml")))
    (if exists-xml?
        (shell-command (concat "bystrotex '" filename "'"))
      (progn
        (message "no registry found, proceeding with single html")
        (shell-command (concat "scribble " filename))))))


(defun amkhlv/scribble/compile-htmls ()
  "Runs scribble -htmls on the file"
  (interactive)
  (save-buffer)
  (let* ((filepath (buffer-file-name)))
    (shell-command (concat "scribble ++arg --htmls --htmls '" filepath "'"))
    )
  )

(defun amkhlv/scribble/view ()
  "This is mapped to C-c C-v in the scribble mode;
 looks up the corresponding .html file and runs firefox on it"
  (interactive)
  (let* ((filepath (buffer-file-name))
         (filename (file-name-nondirectory filepath))
         )
    (shell-command 
     (concat "firefox `bystrotex -l " filename "` &"))))

