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
     (concat "xdg-open `bystrotex -l " filename "` &"))))

(defun amkhlv/image--open (path)
  (if (file-exists-p path)
      (shell-command (concat "inkscape " (shell-quote-argument path)))
    (progn
      (shell-command
       (concat
        "cp ~/.config/inkscape/templates/800x600.svg "
        (shell-quote-argument path)))
      (shell-command (concat "inkscape " (shell-quote-argument path))))))

(defmacro image (&rest args)
  "Create or edit an SVG using Inkscape.

Accepted forms:

  (image PATH)
  (image #:scale SCALE PATH)

The SCALE argument is currently accepted syntactically but ignored."
  (pcase args
    (`(,path)
     `(amkhlv/image--open ,path))
    (`(,key ,_scale ,path)
     (if (and (symbolp key)
              (string= (symbol-name key) "scale"))
         `(amkhlv/image--open ,path)
       (error "Expected #:scale, got %S" key)))
    (_
     (error "Bad syntax, args=%S" args))))


