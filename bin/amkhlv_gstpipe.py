#!/usr/bin/python3

# I used instructions from this page:
# https://adnanalamkhan.wordpress.com/2015/03/01/using-gstreamer-1-0-with-python/


# aptitude install   gstreamer1.0-pulseaudio  gstreamer1.0-plugins-ugly


import gi
gi.require_version('Gst', '1.0')
from gi.repository import GObject,Gtk
from gi.repository import Gst as gst
from collections import OrderedDict
import json
import argparse

GObject.threads_init()
gst.init(None)
pipeline = gst.Pipeline() 

def make_elements(xs):
    result = [gst.ElementFactory.make(xs[i][0], "e" + str(i)) for i in range(len(xs))]
    for i in range(len(xs)):
        for prop in xs[i][1].keys():
            result[i].set_property(prop, xs[i][1][prop])
    return result

def prepare_pipeline(xs):
    for i in range(len(xs)): 
        if (not xs[i]): 
            print("NO " + str(i))
        else:
            pipeline.add(xs[i])
    for n in range(1, len(xs)):
        xs[n-1].link(xs[n])

if __name__ == '__main__' :
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawTextHelpFormatter,
        description='run a GStreamer pipe',
        epilog="""example of JSON:\n [["filesrc", {"location": "/path/to/file.mp3"}],\n  ["mad", {}],\n  ["audioconvert", {}],\n  ["pulsesink", {}]]"""
    )
    parser.add_argument('json_conf_file', help="location of JSON configuration file")
    args = parser.parse_args()

    with open(args.json_conf_file, 'r') as fh:
        f = json.load(fh)

    elms = make_elements(f)
    prepare_pipeline(elms)
    pipeline.set_state(gst.State.PLAYING)

    # Wait until error or EOS.
    bus = pipeline.get_bus()
    msg = bus.timed_pop_filtered(gst.CLOCK_TIME_NONE,gst.MessageType.ERROR | gst.MessageType.EOS)
    print(msg.get_structure())
    print(msg.parse_error())

    # Free resources.
    pipeline.set_state(gst.State.NULL)
