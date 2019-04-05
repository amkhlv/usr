Example start script
====================

    java -Dconfig.file=$(pwd)/data.conf \
         -cp ~/usr/share/sbt/web/target/scala-2.12/web-assembly-0.1.0-SNAPSHOT.jar \
         web.sites.AAA

Writing new robots
==================

They all go into `web.sites`

Template for new robots
-----------------------

is `web.sites.AAA` (just a "minimal example")

Access to config
----------------

is via `web.conf`. 

The configuration files are HOCON. 
Remember that several `-Dconfig.file` can be given on the command line. 
They all get merged

Decryptor
---------

The file to decrypt should be `secretFile` in configuration.
Its decripted contents are obtained by:

    utils.Decryptor.decrypt

Decryption is by `gpg --decrypt`. The keyring manager should be running on the system.

Captcha
=======

See `web.utils.AntiCaptcha`  

Requires configuration key `antiCaptchaClientKey`