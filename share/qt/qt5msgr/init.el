(defun myclargs () 
  (when (equal argi "--outdir")
    (message "setting outdir")
    (shell-command 
     (concat "DISPLAY=:0.0  XAUTHORITY=~/.Xauthority  build-qt5msgr-Desktop-Debug/qt5msgr " (pop command-line-args-left) " &" )
     (generate-new-buffer "incoming")
     )
    t
    ))

(setq command-line-functions (cons 'myclargs command-line-functions))

