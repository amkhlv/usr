Create new secret key
=====================

With salted, hashed password
----------------------------

    gpg --gen-key --s2k-cipher-algo AES256 --s2k-digest-algo SHA512 --s2k-mode 3 --s2k-count 65000000

Nice discussion: http://nullprogram.com/blog/2012/06/24/


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

