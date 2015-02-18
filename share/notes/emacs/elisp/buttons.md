Buttons
=======

Create some buffer and a button in it:


    (setq buf (generate-new-buffer "my-new-buffer"))
    (with-current-buffer buf
      (insert-button "zen" 'action (lambda (x) (start-process "zenity" "zenity-buf" "zenity" "--info" "--text" "hi there"))))

Then go to that new buffer, put the cursor on the link and press __Enter__


