Create new secret key
=====================

With salted, hashed password
----------------------------

First of all, __we should create the folder__ `.gnupg` (because GnuPG for some reason cannot create this folder for us)

Go to the directory wich contains the homedir of `GnuPG` and say:

    gpg --homedir=.gnupg --gen-key --s2k-cipher-algo AES256 --s2k-digest-algo SHA512 --s2k-mode 3 --s2k-count 65000000    

Nice discussion: http://nullprogram.com/blog/2012/06/24/


Change password
---------------

    gpg --s2k-cipher-algo AES256 --s2k-digest-algo SHA512 --s2k-mode 3 --s2k-count 65000000 --edit-key <key id>

and at the prompt say `passwd`

View key parameters
-------------------

    gpg --list-packets ~/.gnupg/secring.gpg

Numbers of algorithms are available here: [RFC-4880 Section 3.7](http://tools.ietf.org/html/rfc4880#section-3.7)


Export keys
===========

Export secret key
-----------------

    gpg --export-secret-keys --armor -a C2E93B4A > key.asc

(without the `-a` switch, exports __all__ secret keys)


Encrypt files
=============

Public key encryption
---------------------

    gpg --homedir /home/andrei/mo2/.gnupg/  -e -r boot-belavista /dev/shm/belavista

This will create the file `/dev/shm/belavista.gpg`

Symmetric encryption
--------------------

(I am not sure that this is the best solution)

Example:

    gpg --symmetric --cipher-algo AES256 -o mo/cryptex.gpg  /dev/shm/cryptex 

The decryption syntax is the same as it was in the asymmetric case:

    gpg -d -o /dev/shm/cryptex mo/cryptex.gpg

