Chrome, Vivaldi
===============

    aptitude install libnss3-tools

Then, as normal user:

    pk12util -d sql:$HOME/.pki/nssdb -i mycert.pfx

Now `chrome` sees this cert !
