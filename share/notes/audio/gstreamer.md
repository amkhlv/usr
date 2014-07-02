# From microphone to headphones on local host

To get `gst-launch` we have to install:

    aptitude install gstreamer-tools

Simplest test with `pulserc`:

    gst-launch pulsesrc ! audioconvert ! autoaudiosink

Working example with `audiotestrc` :

    gst-launch audiotestsrc freq=1000 ! mulawenc ! rtppcmupay ! udpsink host=localhost port=5555
    gst-launch udpsrc port=5555 caps="application/x-rtp" ! queue ! rtppcmudepay ! mulawdec ! audioconvert ! alsasink

Working example with microphone, essentially from [https://labs.isee.biz/index.php/Example\_GStreamer\_Pipelines](https://labs.isee.biz/index.php/Example_GStreamer_Pipelines):

    gst-launch alsasrc  ! mulawenc ! rtppcmupay ! udpsink host=localhost port=5555
    gst-launch udpsrc port=5555 caps="application/x-rtp" ! queue ! rtppcmudepay ! mulawdec ! audioconvert ! alsasink

Generally speaking, see overview of available plugins: [http://gstreamer.freedesktop.org/documentation/plugins.html](http://gstreamer.freedesktop.org/documentation/plugins.html)
where all these `alsasrc`, `udpsrc`, `rtppcmupay` and other words are explained!

Try with vorbis:

    gst-launch alsasrc  ! vorbisenc ! rtpvorbispay ! udpsink host=localhost port=5555
    gst-launch udpsrc port=5555 caps="application/x-rtp" ! queue ! rtpvorbisdepay ! vorbisdec ! audioconvert ! alsasink

\--- does not work, have to think some more

# To remote host

First run `alsamixer` and make sure that the correct sound card is chosen.

Suppose that I have port 8888 open on the machine `192.168.1.103` . Then I use:

    gst-launch alsasrc ! mulawenc ! rtppcmupay ! udpsink host=192.168.1.103 port=8888

And on that machine, in order to listen to the stream, I do:

    gst-launch udpsrc port=8888 caps="application/x-rtp" ! queue ! rtppcmudepay ! mulawdec ! audioconvert ! alsasink

Example with `mp3` (needs `gstreamer-plugins-ugly`):

    gst-launch alsasrc ! audio/x-raw-int,rate=8000,channels=1 ! lame name=enc quality=0 mode=0 vbr=4 vbr-quality=0 ! rtpmpapay ! udpsink host=192.168.1.101 port=8888

    gst-launch udpsrc port=8888 caps="application/x-rtp" ! queue ! rtpmpadepay ! mad ! audioconvert ! alsasink

Or maybe this:

    gst-launch alsasrc ! audio/x-raw-int,rate=32000,channels=1 ! lamemp3enc name=tagger target=bitrate cbr=true bitrate=32 ! rtpmpapay ! udpsink host=192.168.1.101 port=8888

This seems to be the best:

    gst-launch alsasrc ! audio/x-raw-int,rate=8000,channels=1 ! mulawenc ! rtppcmupay ! udpsink host=192.168.1.101 port=8888

    gst-launch udpsrc port=8888 caps="application/x-rtp,clock-rate=(int)8000" ! queue ! rtppcmudepay ! mulawdec ! audioconvert ! alsasink

__Attention__ : if the __receiver__ side has pulseaudio, have to use `pulsesink` instead of `alsasink`
