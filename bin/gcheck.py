#!/usr/bin/python

__author__    = "Andrei Mikhailov"
__copyright__ = "Copyright 2013, Andrei Mikhailov"
__license__   = "GPL v2"

"""A script for watching gmail\n To stop it: run command stopgcheck.py """

# ======================================================================
# This program is based on check-gmail.py by Baishampayan Ghose:
# ======================================================================
# Copyright (C) 2006 Baishampayan Ghose <b.ghose@ubuntu.com>
# Time-stamp: Mon Jul 31, 2006 20:45+0530
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 as
# published by the Free Software Foundation.
# ======================================================================


import os

CONF_FILE = os.environ['HOME'] + "/.config/amkhlv/accounts.yaml"
_URL = "https://mail.google.com/gmail/feed/atom"

WM_WINDOW_CLASS = "zenity"
WM_WINDOW_GEOMETRY = "+300+300"
WINDOW_TIMEOUT = 90

SOUND_FIX    = os.environ['HOME'] + "/a/sounds/chimes.wav"
SOUND_GMAIL  = os.environ['HOME'] + "/a/sounds/ding.wav"
SOUND_GTALK  = os.environ['HOME'] + "/a/sounds/frogs.wav"

HLT_GTALK = "[1;32m" 
HLT_GMAIL = "[43m[30m" 
HLT_OFF   = "[0m"

DB_FILE     = os.environ['HOME'] + "/maildirs/gcheck.sqlite"
DEBUG_FILE  = "/tmp/gcheck-debug.txt"


import sys
import time
import urllib             # For BasicHTTPAuthentication
import feedparser         # For parsing the feed
import Tkinter
import getpass
import sleekxmpp          # For chat
import logging
import threading
import datetime
import subprocess
import sqlite3            # For old messages database
import yaml
import gobject

import dbus
import dbus.service
import dbus.mainloop.glib

do_exit = [False]
threadlist = {}

def aplay(fl):
    os.system("aplay " + fl + " 2>&-")     

def read_yaml(yaml_filename):
    with open(yaml_filename, 'r') as yamlfl:
        y = yaml.safe_load(yamlfl)
        return y

class myURLopener(urllib.FancyURLopener):
    def __init__(self, mylogin):
        self.mylogin = mylogin
        urllib.FancyURLopener.__init__(self)
    mypass = "NOSET"
    def prompt_user_passwd(self, host, realm):
        mypassword = getpass.getpass("My pass:")
        self.mypass = mypassword
        return (self.mylogin, mypassword)

def auth(mylogin):
    '''The method to do HTTPBasicAuthentication'''
    #opener = urllib.FancyURLopener()
    opener = myURLopener(mylogin)
    return opener

def notifier(to_show,oldstrings) :
    def killme(win):
        notify_win.destroy()
    notify_win = Tkinter.Tk(className = WM_WINDOW_CLASS)
    notify_win.after(WINDOW_TIMEOUT*1000, killme, tuple([notify_win]))
    notify_win.wm_geometry(newGeometry = WM_WINDOW_GEOMETRY)
    notify_win.wm_title(string="gmail notify")
    def open_link_in_browser(lnk):
        def inner():
            subprocess.Popen(["firefox",lnk])
            os.system("""wmctrl -a Vimperator  &""")
            print("=========================================================")
            notify_win.destroy()
        return inner
    for x in to_show:
        w = Tkinter.Button(
            notify_win, 
            text = x['TTL'], 
            background = "yellow",
            command = open_link_in_browser(x['LNK'])
            )
        w.pack()
    j=0
    for x in oldstrings:
        j = j+1
        if j<5:
            w = Tkinter.Label(notify_win, text=x)
            w.pack()
        else:
            break
    def goto_firefox():
        os.system("""wmctrl -a Vimperator  &""")
        print("=========================================================")
        notify_win.destroy()
    def ignore():
        notify_win.destroy()
        print("=========================================================")
    b_ok = Tkinter.Button(notify_win, text = "Go to Firefox", command=goto_firefox)
    b_ok.pack()
    b_ignore = Tkinter.Button(notify_win, text = "Ignore", command=ignore)
    b_ignore.pack()
    notify_win.mainloop()

def start_watching_gtalk(opener):
    xmpp = GTalkWatcher(opener.mylogin+"@gmail.com", opener.mypass)
    xmpp.register_plugin('xep_0030') # Service Discovery
    xmpp.register_plugin('xep_0004') # Data Forms
    xmpp.register_plugin('xep_0060') # PubSub
    xmpp.register_plugin('xep_0199') # XMPP Ping
#    if xmpp.connect(('talk.google.com', 5222)):
    if xmpp.connect():
        # If you do not have the dnspython library installed, you will need
        # to manually specify the name of the server if it does not match
        # the one in the JID. For example, to use Google Talk you would
        # need to use:
        #
        # if xmpp.connect(('talk.google.com', 5222)):
        #     ...
        xmpp.process()
    return xmpp

def readmail(opener, signal_array):
    '''Parse the Atom feed and print a summary'''
    link = sqlite3.connect(DB_FILE)
    cursor = link.cursor()
    def parameters_to_compare(ent):
        return (ent.id, ent.date, ent.title, ent.author, ent.summary, ent.link)

    connected = False
    while not(connected):
        try: 
            print("opening " + _URL)
            ff = opener.open(_URL)
            print("=== connected to gmail ==================================")
            aplay(SOUND_GMAIL)
            connected = True
        except IOError:
            os.system("""echo $( date ) --- NO CONNECTION""") 
            time.sleep(10)

    xmpp = start_watching_gtalk(opener)
    atom = feedparser.parse(ff.read())

    print("Continuing")
    j = 0
    # for x in atom.entries:
    #     os.system("""echo """ + x.id + """ >> /tmp/gmailids.txt""")
    for x in atom.entries:
        j = j + 1
        if j<10: 
            print ( x.id + " == " + x.date + ", " + x.author + " -- " + x.title ) 
        else: 
            break
    was_error = False
    bailout = False
    while True:
        # print("MAIN LOOP")
        # Mechanism for breaking out of the loop:
        if signal_array[0]:
            print("Exiting")
            xmpp.disconnect()
            link.close()
            break
        oldentries = list(atom.entries)
        oldstrings = [ ( oldentry.date + ", " + oldentry.author + " -- " + oldentry.title ) for oldentry in oldentries ]
        for en in oldentries:
            exists = list(cursor.execute(
                "select * from oldentries where " +
                " id = ? AND timestamp = ? AND title = ? AND author = ? AND summary = ? AND link = ? " ,
                parameters_to_compare(en)
                )) # Note that ``date'' was renamed ``timestamp''
            if exists:
                pass
            else:
                cursor.execute(
                    "insert into oldentries values (?,?,?,?,?,?) ", parameters_to_compare(en)
                    )
            link.commit()
        # sleeping 20 seconds:
        for n in range(20):
            if signal_array[0]:
                print("Exiting while waiting")
                bailout = True
                xmpp.disconnect()
                link.close()
                break
            else:
                time.sleep(1)
        if bailout : break
        #print("WILL OPEN URL")
        try:
          ff = opener.open(_URL)
          atom = feedparser.parse(ff.read())
          # DEBUG:
          with open(DEBUG_FILE, 'a') as fh:
              fh.write("\n=========================================================\n")
              fh.write(str(datetime.datetime.now()))
              fh.write("\n=========================================================\n")
              for x in atom.entries:
                  fh.write( x.date + ", " + x.author + " -- " + x.title + "\n" )
          # report that error was fixed:
          if was_error:
             was_error = False
             print(str(datetime.datetime.now()) + ": connection fixed")
             aplay(SOUND_FIX)
          def unseen(entry): 
              oldvals = list(cursor.execute(
                "select * from oldentries where " +
                " id = ? AND timestamp = ? AND title = ? AND author = ? AND summary = ? AND link = ? " ,
                parameters_to_compare(entry)
                )) # Note that ``date'' was renamed ``timestamp''
              if oldvals: is_new = False
              else: is_new = True
              return is_new
          newentries = [ entry for entry in atom.entries if unseen(entry) ]
          to_show = [ {'TTL': newentry.date + ", " + newentry.author + " -- " + newentry.title ,
                       'LNK': newentry.link} for newentry in newentries ]
          if to_show: aplay(SOUND_GMAIL)
          for s in to_show: 
              print(HLT_GMAIL + s['TTL'] + HLT_OFF)
          if to_show:
              notifier_thread = threading.Thread(target = notifier, 
                                                 args = tuple([to_show, oldstrings]))
              notifier_thread.start()
        except:
             if not(was_error): 
                 was_error = True
                 print("Detected error:", sys.exc_info()[0])

# This was taken from  http://sleekxmpp.com/getting_started/echobot.html 
class GTalkWatcher(sleekxmpp.ClientXMPP):

    """
    A simple SleekXMPP bot that will echo messages it
    receives
    """

    def __init__(self, jid, password):
        sleekxmpp.ClientXMPP.__init__(self, jid, password)

        # The session_start event will be triggered when
        # the bot establishes its connection with the server
        # and the XML streams are ready for use. We want to
        # listen for this event so that we we can initialize
        # our roster.
        self.add_event_handler("session_start", self.start)

        # The message event is triggered whenever a message
        # stanza is received. Be aware that that includes
        # MUC messages and error messages.
        self.add_event_handler("message", self.message)

    def start(self, event):
        """
        Process the session_start event.

        Typical actions for the session_start event are
        requesting the roster and broadcasting an initial
        presence stanza.

        Arguments:
            event -- An empty dictionary. The session_start
                     event does not provide any additional
                     data.
        """
        self.send_presence()
        self.get_roster()

    def message(self, msg):
        """
        Process incoming message stanzas. Be aware that this also
        includes MUC messages and error messages. It is usually
        a good idea to check the messages's type before processing
        or sending replies.

        Arguments:
            msg -- The received message stanza. See the documentation
                   for stanza objects and the Message stanza to see
                   how it may be used.
        """
        if msg['type'] in ('chat', 'normal'):
            print(HLT_GTALK + 
                  str(datetime.datetime.now()) + ":  " + 
                  msg['from'].jid + " says: " + msg['body'] +
                  HLT_OFF)
            aplay(SOUND_GTALK)
        else:
            print("got message")

class GCheckObject(dbus.service.Object):
    def __init__(self, x, y, some_array, mainloop):
        self.signal_array = some_array
        self.mainloop  = mainloop
        dbus.service.Object.__init__(self, x, y)
    @dbus.service.method("amkhlv.GCheckInterface",
                         in_signature='',
                         out_signature='')
    def ExitGCheck(self):
        print("--- Received EXIT signal ---")
        self.signal_array[0] = True
        self.mainloop.quit()

def startDBusService():
    gobject.threads_init()
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
    session_bus = dbus.SessionBus()
    name = dbus.service.BusName("amkhlv.GCheck", session_bus)
    mainloop = gobject.MainLoop()
    object = GCheckObject(session_bus, '/GCheckObject', do_exit, mainloop)
    print("Starting dbus service...")
    mainloop.run()


if __name__ == "__main__":
    mylogin = read_yaml(CONF_FILE)['gmail']['login']
    logging.basicConfig(level=logging.ERROR,
                        format='%(levelname)-8s %(message)s')

    o = auth(mylogin)  # Do auth and then get the feed
    aplay(SOUND_FIX)
    t1 = threading.Thread(target=startDBusService)
    t2 = threading.Thread(target=readmail, args=(o, do_exit))
    threadlist['dbus'] = t1; threadlist['readmail'] = t2
    t1.start()
    t2.start()
