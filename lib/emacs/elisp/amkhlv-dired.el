(provide 'amkhlv-dired)

(defun auxhide () 
"This hides auxiliary files in dired."
(interactive)
  (dired-mark-files-regexp "_region_\....") 
  (dired-mark-files-regexp ".*\.fmt$")
  (dired-mark-files-regexp ".*\.prv$")
  (dired-mark-files-regexp ".*\.log$")
  (dired-mark-files-regexp ".*\.aux$")
  (dired-mark-files-regexp ".*~$")
  (dired-do-kill-lines)
)

(defun my-org-dired-update ()
"This reverts the dired buffer and then hides aux files.
Bound to C-c h in dired"
(interactive)
  (revert-buffer)
  (auxhide)
)
