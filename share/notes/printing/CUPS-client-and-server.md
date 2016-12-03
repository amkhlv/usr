
Server setup
============

Need to edit `/etc/cups/cupsd.conf`.

Have to add a `Listen` line, for example:

  Listen 192.168.101.1:631

and an `Allow` line:

  Allow 192.168.101.111

Then restart:

  systemctl restart cups

Also need to allow `tcp` to port `631` in `iptables`

Client setup
============

Source: [debianadmin](http://www.debianadmin.com/setup-cups-common-unix-printing-system-server-and-client-in-debian.html)

Need to __create__ the file `/etc/cups/client.conf` with the following contents:

  # Servername
  ServerName 192.168.101.1

  # Encryption
  Encryption IfRequested


Use
===

Standard commands on the client machine (such as `lpstat -a`) will just use the server.
