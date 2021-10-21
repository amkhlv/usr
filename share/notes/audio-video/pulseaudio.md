Starting and stopping
=====================

To stop:

    pulseaudio -k

To start in verbose diagnostic mode:

    pulseaudio -vvvv

To start as a daemon:

    pulseaudio -D

To check status:

    pulseaudio --check
    echo $?

(`0` means running, `1` means not running)

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





CLI
===

Main commands
-------------

The main command is `pacmd` ; the most useful subcommands to start with are:

    pacmd help

and

    pacmd dump

The second one, `pacmd dump`, is very nice: it prints a list of lines which need to be input (prefixed with `pacmd`) to arrive at the current state.
For example `set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo 0xdeae` is achieved by executing:

    pacmd set-sink-volume alsa_output.pci-0000_00_1f.3.analog-stereo 0xdeae

Moving sources and sinks
------------------------

Suppose that I have some application running (like `Firefox`) and I want to transfer its audio output stream over Internet.

I execute [amkhlv_gstpipe.py](../../../bin/amkhlv_gstpipe.py) to establish a `gstreamer` pipe to some remote location.
The entry of this pipe is linked to some `pulseaudio` output, which outputs audio from some source. To figure out what
is this source output, execute:

    pacmd list-source-outputs

As far as I understand, this actually __lists pairs__ `(source-output,client)`. In particular, `amkhlv_gstpipe.py` is listed,
let us say, under `index: 4`:

    index: 4
        ...
        properties:
                media.name = "Record Stream"
                application.name = "amkhlv_gstpipe.py"

In fact, I can separately list clients:

    pacmd list-clients

This would list all clients, __index__-ed by a number, one of them will have `application.name = "amkhlv_gstpipe.py"` --- this is my pipe.
Now, returning to `list-source-outputs`, by looking at the output I can figure out that it receives from, for example, 
`source: 1 <alsa_input.pci-0000_00_1f.3.analog-stereo>`     (which is actually a microphone, as we can see from the word ``input'').
This means that, at the moment, the remote machine is listening to our microphone... 

Instead, I want to feed it the output from the running `Firefox` (maybe YouTube...). So, I look again at the output of `list-source-outputs`,
and I see that it includes some `source: 0 <alsa_output.pci-0000_00_1f.3.analog-stereo.monitor>` . This is, actually, the ``monitor''
(a side channel) of the audio card output. So, I have to move __4__ from __1__ to __0__ :

    pacmd move-source-output 4 0

And, if I want to move it back to the microphone, I execute:

    pacmd move-source-output 4 1

Similarly, there are commands `list-sink-inputs` and `move-sink-input`.

The __GUI__ for all this is `pavucontrol`

Virtual sound card
------------------

    modprobe snd-aloop

creates a virtual sound card called `Loopback 1`

Over network
============

Adding sink locally
-------------------

First have to enable network using `paprefs` 

    pactl load-module module-tunnel-sink "server=192.168.1.105 sink=alsa_output.pci-0000_00_1b.0.analog-stereo sink_name=home_theater"

where `alsa_output.pci-0000_00_1b.0.analog-stereo` was obtained by running, on that machine:

    pacmd list-sinks

Same with sources.


Using remote PULSE_SERVER
-------------------------

This solution seems to be easier than [Adding sink locally], and works for microphone too. (But then, need to specify `PULSE_SERVER` when starting Firefox _etc_)

Copy `/etc/pulse/default.pa` to `~/.pulse/default.pa` and add these lines there:

    load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1 auth-cookie=/home/pi/.config/pulse/cookie

Need also to run:

    paprefs

and allow discovery over network.

Then, on the client, just say:

    PULSE_SERVER=192.168.x.y whatever 

For example, to test recording:

    parecord -s 192.168.x.y output.wav

(Here `192.168.x.y` is the address of the __server__)

### Authentication

I think it authenticates with a cookie. The file:

    ~/.config/pulse/cookie

should be __same on client and server__



Using UNIX domain socket
========================

Usually, `PulseAudio` uses `shm` istead of `UNIX socket`, so it is not available. This means that we need to __disable the use of shm__ by `PulseAudio`.

As I [explained previously](#confusing-defuzzing) , this can be done by creating the file `/home/andrei/.pulse/daemon.conf` and putting a line in it:

    disable-shm=yes

Moreover, I need to export the environment variable:

    export XDG_RUNTIME_DIR=/run/user/1000

and make sure that it exists and has right permissions:

    mkdir -p /run/user/1000/pulse
    chown -R andrei:andrei /run/user/1000

(I usually do it in `~/.bashrc`)

Then, after the restart of `PulseAudio`, we get that `UNIX socket` available. 


Authentication
==============

I think it authenticates with a cookie. The file:

    ~/.config/pulse/cookie

should be __same on client and server__
