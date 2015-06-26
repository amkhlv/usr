Securing socket
===============

In `/etc/dovecot/dovecot.conf` have a line:

    listen = 127.0.0.1


Authentication
==============

Run as your own non-root user:

    echo "$USER:{PLAIN}password:$UID:$GID::$HOME" > users
    sudo mv users /etc/dovecot/
    sudo chown root:dovecot users
    sudo chmod o-r users

You can (and should) replace the "password" with whatever password you wish to use, but don't use any important password here as we'll be logging in with insecure plaintext authentication until SSL is configured.

If you used the example configuration files, switch to passwd-file by modifying `conf.d/10-auth.conf`:

    # Add '#' to comment out the system user login for now:
    #!include auth-system.conf.ext

    # Remove '#' to use passwd-file:
    !include auth-passwdfile.conf.ext

In `conf.d/auth-passwdfile.conf.ext` you should have:

    passdb {
      driver = passwd-file
      args = scheme=CRYPT username_format=%u /etc/dovecot/users
    }
    userdb {
      driver = passwd-file
      args = username_format=%u /etc/dovecot/users
    }

Verify with `doveconf -n passdb userdb` that the output looks like above (and there are no other passdbs or userdbs). 


Connect by telnet
=================

    telnet localhost 143
    a login username password
    e logout


More info [here](http://wiki2.dovecot.org/TestInstallation)

