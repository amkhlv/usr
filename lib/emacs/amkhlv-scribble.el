(provide 'amkhlv-scribble)

(defun amkhlv/scribble/compile ()
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
                    (shell-command (concat "scribble ++arg --dest --dest " (cdr (assoc 'dest h)) " " filename)))
                (progn
                  (message (concat "scribble " filename))
                  (shell-command (concat "scribble " filename))))))
                                        ; Then iterate over --htmls :
          (dolist
              (h (mapcar (lambda (x) x) ; this is to convert vector to list
                         (cdr (assoc 'htmls registry))))
            (when (string= filename (concat h ".scrbl")) 
              (message (concat "scribble  multipage " filename))
              (shell-command (concat "scribble ++arg --htmls --htmls " filename)))))
      (progn
        (message "no registry found, proceeding with single html")
        (shell-command (concat "scribble " filename))))))

(defun amkhlv/scribble/compile-htmls ()
  "Runs scribble -htmls on the file"
  (interactive)
  (save-buffer)
  (let* ((filepath (buffer-file-name)))
    (shell-command (concat "scribble ++arg --htmls --htmls " filepath))
    )
  )

(defun amkhlv/scribble/view ()
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

