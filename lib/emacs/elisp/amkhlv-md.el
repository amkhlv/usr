(provide 'amkhlv-md)

(defun markdown-to-html ()
  (interactive)
  (let* ((fn buffer-file-name)
	 (fnout (replace-regexp-in-string "md$" "html" fn)))
    (save-buffer)
    (shell-command (concat "md2html " fn " > " fnout))))

(defun markdown-view-html ()
  (interactive)
  (let* ((fn buffer-file-name)
	 (fnout (replace-regexp-in-string "md$" "html" fn)))
    (shell-command (concat "firefox " fnout " &"))))
