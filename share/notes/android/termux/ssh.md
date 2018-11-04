
Access via SSH
==============

    pkg install openssh

Then populate `.ssh/authorized_keys`

To use:

    sshd
    
will listen on Port `8022`

To log to console:

    logcat -s 'syslog:*'

To stop:

    pkill ssh

