Configuration
=============

Edit `/etc/ircd-hybrid/ircd.conf`

Section `serverinfo`
--------------------

Choose some server name. Also, don't foget to change `sid` to a random number

Section `admin`
---------------

        name = "Andrei Mikhailov";
        description = "Main Server Administrator";
        email = "<root@localhost>";

Section `listen`
----------------

Specify the IP address to listen on as `host`

Section `auth`
--------------

__Comment out__ the line: `flags = need_ident;`
if it is present. (We do not want the identd)

Section `general`
-----------------

`disable_auth = yes;`

Section `operator`
------------------

This is to specify the credential which are asked when entering the command `/oper` in `irssi`.
The password is hashed, the hash is obtained by running the command:

    /usr/bin/mkpasswd --method=sha-256

Random `udp` port
=================

`ircd-hybrid` opens a random `udp` port, which is for the DNS.

Configuring the interaction with services
=========================================

The services such as `nickserv` and `chanserv` are provided by an
external program, in our case `anope`.

See [my writeup on anope](anope.html) about configuring the service on the `anope` side.
But we also have to configure on the side of `ircd-hybrid`. The corresponding section
is called `connect`.

Sections `connect` and `shared`
-------------------------------

    connect {
     name = "services.andreimikhailov.com";
     host = "192.168.88.1";
     send_password = "mypassword-1";
     accept_password = "mypassword-1";
     encrypted = no;
     class = "server";
     hub_mask = "*";
    };
    shared {
      name = "services.andreimikhailov.com";
    };

There is an excellent auto-generator [here](http://www.anope.org/ilm.php?p=4&i=hybrid) (have to click on the link `Click Here` on
that page).





