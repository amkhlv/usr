(with-temp-buffer
        (insert-file-contents (expand-file-name "~/.mailaliases"))
        (goto-char (point-min)) ;; beginning-of-buffer (but faster)
        (while
    	(looking-at
    	 "alias[^\"]*\"\\([^\"]*\\)\"[^<].\\([^>]*\\)")
        (message (buffer-substring (match-beginning 1) (match-end 1))) 
        (eval '(bbdb-create-internal
                (buffer-substring (match-beginning 1) (match-end 1)) nil
                (buffer-substring (match-beginning 2) (match-end 2)) nil nil nil) )
        (forward-line)))
