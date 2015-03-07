(provide 'amkhlv-nxml)

(defun rng-reload-schema ()
  (interactive)
  (let ((schema-filename rng-current-schema-file-name))
    (when schema-filename
      (setq rng-current-schema (rng-load-schema schema-filename))
      (run-hooks 'rng-schema-change-hook)
      (message "Reloaded schema %s" schema-filename))
    (unless schema-filename
      (rng-set-schema-and-validate))))

(defun rng-apply-find-schema-file (fn)
  (let ((schema-filename rng-current-schema-file-name))
    (unless schema-filename
      (error "This file is not associated with a schema file."))
    (funcall fn schema-filename)))

(defun rng-find-schema-file ()
  (interactive)
  (rng-apply-find-schema-file 'find-file))

(defun rng-find-schema-file-other-window ()
  (interactive)
  (rng-apply-find-schema-file 'find-file-other-window))
