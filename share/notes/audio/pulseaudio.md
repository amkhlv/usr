
Confusing-defuzzing
===================

<a name="sectionColinGuthr"></a>

From "[confusing-defuzzing](http://colin.guthr.ie/2009/08/sound-on-linux-is-confusing-defuzzing-part-2-pulseaudio/)":

The first thing a PulseAudio client has to do is connect to a server.
In order to do this, it checks various variables and configuration files to determine precisely to __which__ server it will connect!

Initially, the PulseAudio client library looks for a `PULSE_SERVER` environment variable.
If found, this variable can define a list of servers to which the client should connect.
These servers can be specified as local UNIX sockets or DNS names/IP addresses for a TCP connection.

Assuming it's still not got a server to connect to yet, PulseAudio will check for a default-server
configuration in it's `client.conf` file. This file is located in either `/etc/pulse/` or `~/.pulse/`
(Only one of the client.conf files is parsed. So if the user has their own one, the system one will not be parsed at all.)


Forwarding across network
=========================

Suppose that `MachineA` (server) needs to run an application with sound but does not have speakers, while
`MachineB` (client) has speakers.

We modify the description on [Joshua Tauberer’s Blog](https://razor.occams.info/blog/2009/02/11/pulseaudio-sound-forwarding-across-a-network/)
by running port forwarding in reverse and using the standard on Debian `PulseAudio` socket `/run/user/1000/pulse/native`:

On `MachineB` (client):

    ssh -N -f -R4000:localhost:4000 machineA.com
    socat TCP-LISTEN:4000,fork UNIX-CONNECT:/run/user/1000/pulse/native

On `MachineA` (server):

    PULSE_SERVER=localhost:4000  mysoundapplication

This, of course, requires the availability on `MachineB` of the `UNIX socket`:

    /run/user/1000/pulse/native

Usually, `PulseAudio` uses `shm` istead of `UNIX socket`, so it is not available. This means that we need to __disable the use of shm__ by `PulseAudio`.

As I [explained previously](#sectionColinGuthr) , this can be done by creating the file `/home/andrei/.pulse/` and putting a line in it:

    disable-shm=yes

Then, after the restart of `PulseAudio`, we get that `UNIX socket` available. 
