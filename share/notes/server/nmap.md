Options summary
===============

To see the options summary, simply run:

    nmap


Simple scans
============

No ping
-------

Option `-PN` means do not ping first (because many times they do not respond to ping)
For example, to scan port 9100 (which is JetDirect of a printer):

    nmap -PN 172.16.10.1 -p 9100

Agressive
---------

May be illegal, takes lots of time:

    nmap -A 172.16.10.1

The option `-A` enables OS detection, version detection, script scanning, and traceroute. The result
of such scan provides __tons of useful info__

Ping scan
---------

Just a simple ping scan of everything on 192.168.1.* :

    nmap -sP 192.168.1.0/24

TCP SYN scan
------------

    nmap -sS 172.16.10.1

SYN scan is __the default__ and most popular scan option for good reasons. It can be performed quickly,
scanning __thousands of ports per second__ on a fast network __not hampered by restrictive firewalls__.
It is relatively unobtrusive and stealthy.
It allows clear, reliable differentiation between the open, closed, and filtered states.

You don't open a full `TCP` connection. You send a `SYN` packet. A `SYN/ACK` indicates the port is
listening (open), while a `RST` (reset) is indicative of a non-listener. If no response is received
after several retransmissions, the port is marked as filtered. The port is also marked filtered if
an `ICMP` unreachable error is received.
The port is also considered open if a `SYN` packet (without the `ACK` flag) is received in response.


Set a timing template
---------------------

    nmap -T4 scanme.nmap.org


`nmap` has six "preset" timing templates. You can specify them with the `-T` option and their
number (0–5) or their name. The template names are:

__paranoid (0), sneaky (1), polite (2), normal (3), aggressive (4), and insane (5)__

The first two are for IDS evasion. Polite mode slows down the scan to use less bandwidth and
target machine resources. Normal mode is the default and so `-T3` does nothing. Aggressive mode
speeds scans up by making the assumption that you are on a reasonably fast and reliable network.
Finally insane mode assumes that you are on an extraordinarily fast network or are willing to sacrifice some accuracy for speed.

Normally, __just use -T4__



