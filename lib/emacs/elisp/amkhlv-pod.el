(provide 'amkhlv-pod)

(defun mypod-compile ()
  "Runs docconv.sh on the /doc/ folder containing 
 the current .pod file"
  (interactive)
  (save-buffer)
  (let*  ((filepath (buffer-file-name))
          (rootpath (progn
                      (string-match ".*/doc/" filepath)
                      (match-string 0 filepath)
                      )
                    ))
    (shell-command (concat "docconv.sh " rootpath))
    )
  )

(defun mypod-view ()
  "This is mapped to C-c C-v in the pod mode;
 looks up the corresponding .html file in html-doc
 and runs firefox on it"
  (interactive)
  (let* ((filepath (buffer-file-name))
         (rootpath (progn
                     (string-match "\\(.*/\\)doc/" filepath)
                     (match-string 1 filepath)
                     )
                   )
         (fromroot (progn
                     (string-match ".*/doc/\\(.*?\\)\.pod$" filepath)
                     (match-string 1 filepath)
                     )
                   )
         (htmlpath (concat rootpath "html-doc/" fromroot))
         )
    (shell-command (concat "firefox "  htmlpath  ".html &"))
    )
  )

