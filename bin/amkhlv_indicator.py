#!/usr/bin/env python3

from gi.repository import Gtk, Gdk, GObject, GdkPixbuf
from array import array
from multiprocessing import Process, Manager
from threading import Thread
import subprocess
import re
import time


INDICATOR="/home/andrei/.config/amkhlv/indicator.txt"
# This is the default colors file, of the form, e.g. (all squares same color):
# 100 100 80
# 100 100 80
# 100 100 80
# 100 100 80
DEFAULT_COLOR=(80,80,120)

def mk_square_loader(colors):
    pxls = array('H')
    def two_squares(ctr1, ctr2):
        for i in range(20):
            pxls.extend(c for c in (0,0,0))
        for i in range(8):
            pxls.extend(c for c in (0,0,0))
            for j in range(8):
                pxls.extend(int(c) for c in ctr1)
            pxls.extend(c for c in (0,0,0))
            pxls.extend(c for c in (0,0,0))
            for j in range(8):
                pxls.extend(int(c) for c in ctr2)
            pxls.extend(c for c in (0,0,0))
        for i in range(20):
            pxls.extend(c for c in (0,0,0))
    two_squares(colors[:3],   colors[3:6])
    two_squares(colors[6:9], colors[9:12])
    head = b'P6 20 20 65535 '
    loader = GdkPixbuf.PixbufLoader.new_with_type('pnm')
    loader.write(head + pxls)
    return loader

class AmkhlvIndicator(GObject.GObject):
    def __init__(self, sq):
        GObject.GObject.__init__(self)
        self.square = sq
        self.ind = Gtk.StatusIcon()
        self.set_indicator(sq)
        self.ind.connect("popup-menu", self.right_click_event)
        self.timeout = GObject.timeout_add(500, self.on_timeout, None)
        self.ind.set_visible(True)
        self.lightup = True
    def set_indicator(self, square):
        loader = mk_square_loader(square)
        self.ind.set_from_pixbuf(loader.get_pixbuf())
        loader.close()
    def quit_fn(self, user_data):
        Gtk.main_quit()
    def on_timeout(self, user_data):
        if self.lightup:
            self.set_indicator(self.square)
        else:
            loader = mk_square_loader(list(DEFAULT_COLOR)*4)
            self.ind.set_from_pixbuf(loader.get_pixbuf())
            loader.close()
        self.lightup = not(self.lightup)
        return True
    def right_click_event(self, icon, button, time):
        self.menu = Gtk.Menu()
        about = Gtk.MenuItem()
        about.set_label("About")
        quit = Gtk.MenuItem()
        quit.set_label("Quit")
        about.connect("activate", self.show_about_dialog)
        quit.connect("activate", self.quit_fn)
        self.menu.append(about)
        self.menu.append(quit)
        self.menu.show_all()
        def pos(menu, icon):
                return (Gtk.StatusIcon.position_menu(menu, icon))
        self.menu.popup(None, None, pos, self.ind, button, time)
    def show_about_dialog(self, widget):
        about_dialog = Gtk.AboutDialog()
        about_dialog.set_destroy_with_parent(True)
        about_dialog.set_name("AmkhlvIndicator")
        about_dialog.run()
        about_dialog.destroy()

def control(sq):
    while True:
        subprocess.call(["inotifywait", "-e", "close_write", INDICATOR])
        print("updating manager\n")
        with open(INDICATOR, 'r') as fh:
            lns = [ln for ln in fh][:4]
            xs = [int(c) for ln in lns for c in re.split('\W+',ln.rstrip())]
            for i in range(12):
                sq[i] = xs[i]

if __name__ == '__main__':
    manager = Manager()
    square  = manager.list(list(DEFAULT_COLOR)*4)
    p = Process(target = control, args=(square,))
    p.start()
    ind = AmkhlvIndicator(square)
    Gtk.main()
    p.terminate()


