
Generating locales
==================

Edit the file `/etc/locale.gen` and uncomment those which you need, and then:

    locale-gen

For some reason, it is also useful to run:

    dpkg-reconfigure locales

