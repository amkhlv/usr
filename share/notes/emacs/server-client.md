Server with multiple sockets
============================

To start on named socket
------------------------

    emacs --daemon=work

where `work` is the socket name which I choose.

To connect to it
----------------

    emacsclient -s work -c
    
(where `-c` is to start GUI)

To kill running server
----------------------

    emacsclient -s work -e '(save-buffers-kill-emacs)'


