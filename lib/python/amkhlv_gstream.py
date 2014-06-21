#!/usr/bin/env python3

__author__ = "Andrei Mikhailov"
__copyright__ = "Copyright 2014, Andrei Mikhailov"
__license__ = "GPL"

import gi
gi.require_version('Gst', '1.0')
from gi.repository import GObject, Gst, Gtk

def pipeline_stop(pipeline):
    pipeline.set_state(Gst.State.PAUSED)
    return False

if __name__ == '__main__':

    Gst.init(None)
    GObject.threads_init()

    pipeline = Gst.Pipeline()

    # source = Gst.ElementFactory.make('filesrc', None)
    # source.set_property("location", "/home/andrei/allegro.mp3")
    source = Gst.ElementFactory.make('alsasrc', None)
    source.set_property("device","hw:2,0")
    # source = Gst.ElementFactory.make('pulsesrc', None)
    # source = Gst.ElementFactory.make('autoaudiosrc', None)
    # source = Gst.ElementFactory.make('audiotestsrc', None)
    # source.set_property("freq", 400)
    queue = Gst.ElementFactory.make('queue', None)
    # acvt = Gst.ElementFactory.make("audioconvert", "audioconverter")
    # pipeline.add(acvt)
    # sink   = Gst.ElementFactory.make('pulsesink', None)
    caps = Gst.caps_from_string("audio/x-raw,rate=(int)8000,channels=1")
    capsfilter = Gst.ElementFactory.make('capsfilter', None)
    capsfilter.set_property("caps",caps)
    mulawenc = Gst.ElementFactory.make('mulawenc', None)
    rtppcmupay = Gst.ElementFactory.make('rtppcmupay', None)
    rtpL16 = Gst.ElementFactory.make('rtpL16pay', None)
    sink = Gst.ElementFactory.make('udpsink', None)
    #sink.set_property("host", "localhost")
    #sink.set_property("port", 5555)
    sink.set_property("host", "192.168.88.103")
    sink.set_property("port", 8888)
    mad = Gst.ElementFactory.make('mad',None)

    convert = Gst.ElementFactory.make("audioconvert", None)
    resample = Gst.ElementFactory.make("audioresample", None)

    pipeline.add(source)
    pipeline.add(convert)
    pipeline.add(resample)
    pipeline.add(capsfilter)
    pipeline.add(mulawenc)
    pipeline.add(rtppcmupay)
    pipeline.add(rtpL16)
    pipeline.add(sink)
    pipeline.add(mad)
    pipeline.add(queue)

    source.link(capsfilter)
    capsfilter.link(mulawenc)
    mulawenc.link(rtppcmupay)
    rtppcmupay.link(queue)
    queue.link(sink)

    bus = pipeline.get_bus()
    print("GOT bus")
    ret = pipeline.set_state(Gst.State.PLAYING)
    print(ret)
    loop = GObject.MainLoop()
    loop.run()
