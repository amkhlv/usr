
NTP
===

Install the CLI tool
--------------------

To use the command `ntpdate` have to install it:

    aptitude install ntpdate

Forcing time set
----------------

    systemctl stop ntp
    ntpdate -s ntp.unesp.br
    systemctl start ntp

Debugging
---------

    ntpdate -dv pool.ntp.org

