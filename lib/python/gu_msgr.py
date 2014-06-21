#!/usr/bin/env python3

__author__ = "Andrei Mikhailov"
__copyright__ = "Copyright 2014, Andrei Mikhailov"
__license__ = "GPL"

from gi.repository import Gtk, Gdk, GObject
import os
from threading import Thread
import subprocess

class Parameters:
    nlines = 14

def register_css(css_filename):
    """
    To register a CSS style file

    :param str css_filename:
    """
    style_provider = Gtk.CssProvider()

    css = open(os.path.expanduser(css_filename), 'rb') # rb needed for python 3 support
    css_data = css.read()
    css.close()
    css_data = css_data


    style_provider.load_from_data(css_data)

    Gtk.StyleContext.add_provider_for_screen(
        Gdk.Screen.get_default(),
        style_provider,
        Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
    )

class MainWin(Gtk.Window):
    def __init__(self, infile):
        """
        our main window

        :param str infile: filename to watch
        :return:
        """
        self.infile = infile
        self.happy = True
        Gtk.Window.__init__(self, title="*MESSAGE*", name="GuMsgrMainWin")
        self.mainVBox = Gtk.VBox()
        self.text = Gtk.TextView(name="GuMsgrTextView")
        self.text.set_editable(False)
        self.text.set_can_focus(False)
        self.text_buffer = self.text.get_buffer()
        text_style = self.text.get_style_context()
        text_font  = text_style.get_font(Gtk.StateFlags.NORMAL)
        self.set_text_from_file()
        self.line = Gtk.Entry(name="GuMsgrEntry")
        self.line.connect("activate", self.entry_fn)
        self.bottom_label = Gtk.Label(name="GuMsgrBottomLabel")
        self.bottom_label.set_alignment(0,0.5)
        self.mainVBox.add(self.text)
        self.mainVBox.add(self.line)
        self.mainVBox.add(self.bottom_label)
        self.add(self.mainVBox)
        self.connect("delete-event", self.exit_fn)
        self.show_all()
    def scroll_to_end(self,w):
        textMark = Gtk.TextMark(name="mark", left_gravity=False)
        self.text_buffer.add_mark(textMark, self.text_buffer.get_end_iter())
        self.text.scroll_mark_onscreen(textMark)
        self.text_buffer.delete_mark(textMark)
    def entry_fn(self,a):
        txt = self.line.get_text()
        print(txt)
        self.line.set_text("")
        self.bottom_label.set_text(txt)
    def exit_fn(self,a=None,b=None):
        self.happy = False
        if a: a.destroy()
        Gtk.main_quit()
    def set_text_from_file(self):
        #Gdk.threads_enter()
        with open(self.infile, "r") as fh:
            nt = fh.read()
            self.text_buffer.set_text(
                "\n".join(("\n" * Parameters.nlines + nt).split("\n")[- Parameters.nlines:])
            )
        #Gdk.threads_leave()
    def cli(self):
        while self.happy:
            x = input()
            b = self.text_buffer
            if x.find("/quit") == 0:
                self.exit_fn()
            elif x.find("/re") == 0:
                self.set_text_from_file()
            else:
                with open(self.infile, "a") as fh:
                    fh.write(x+"\n")
                self.set_text_from_file()

if __name__ == "__main__":
    GObject.threads_init()
    register_css(os.environ['HOME']+"/.config/amkhlv/gu_msgr.css")
    mw = MainWin(os.environ['HOME']+"/eraseme.txt")
    t = Thread(group = None, target = mw.cli, name="CLI")
    t.start()
    Gtk.main()

