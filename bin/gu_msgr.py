#!/usr/bin/env python3

__author__ = "Andrei Mikhailov"
__copyright__ = "Copyright 2014, Andrei Mikhailov"
__license__ = "GPL"

import tkinter
import IPython
import os
from threading import Thread

class Parameters:
    nlines = 14

class MyText(tkinter.Text):
    def rm(self, n):
        self.delete(str(n)+".0",str(n)+".end+1c")
    def show(self):
        nlines = int(str.split(self.index(tkinter.END),'.')[0])
        for ln in range(1, nlines):
            print(str(ln) + ". " + self.get(str(ln)+".0", str(ln)+".end"))
    def say(self, x):
        self.insert(tkinter.END, "\n" + x)
    def scroll(self):
        self.rm(1)



def start(options):
    """This function constructs GUI with the initial text --- the string argument"""
    root = tkinter.Tk()
    root.title("Hi !")
    from IPython.lib.inputhook import enable_tk; enable_tk(root)
    grid_frame = tkinter.Frame(root)
    rows = ["", "", "", options.helpline if options.helpline else "type here and press ENTER:"]
    lbls = [tkinter.Label(
        grid_frame,
        text=x,
        font = "DejaVu 14"
              )
            for x in rows]
    ent  = tkinter.Entry(
        grid_frame,
        width=60,
        background='yellow',
        font = "DejaVu 14"
        )
    def def_cb(rows):
        def cb(event, rows=rows):
            x = ent.get()
            rows.append(x)
            del rows[0]
            for u in zip(lbls,rows):
                u[0].config(text = u[1])
            with open(options.in_file, "a") as f:
                f.write(x+"\n")
            ent.delete(0,tkinter.END)
        return cb
    ent.bind("<Return>", def_cb(rows))
    ent.focus_set()
    my_t = MyText(
        root,
        width=60,
        height=14,
        font = "DejaVu 14"
        )

    my_t.pack()
    grid_frame.pack(side=tkinter.LEFT)
    r = 0
    for l in lbls:
        l.grid(row=r, sticky=tkinter.W)
        r = r + 1
    ent.grid(row=r, sticky=tkinter.W)

    return root, my_t



if __name__ == "__main__":

    from optparse import OptionParser
    usage = """
    
    %prog -d :0.0 -x .Xauthority -i eraseme.txt --hl 'type something and press Enter'

    You've got objects:
        w : the Tk root window,
        m : the text widget

        m.say("Opa!") add line of text
        m.scroll() removes the first line
        m.rm(n)    removes the n-th line
        m.show()   lists all the lines
    """
    parser = OptionParser(usage=usage)
    parser.add_option("-i", "--infile", dest="in_file", metavar="IN_FILE", help="""specify inbox file""")
    parser.add_option("-d", "--display", dest="display", metavar="DISPLAY")
    parser.add_option("-x", "--xauthority", dest="xauthority", metavar="XAUTHORITY")
    parser.add_option("--hl", dest="helpline", metavar="HELPLINE")
    (options, args) = parser.parse_args()

    if options.display: os.environ['DISPLAY'] = options.display
    if options.xauthority: os.environ['XAUTHORITY'] = options.xauthority

    a = "hello world"
    w,m = start(options)
    cfg = IPython.config.loader.Config()
    cfg.InteractiveShellApp.gui = "tk"
    cfg.InteractiveShell.autocall = 2

    def ipmain():
        IPython.embed(config=cfg)
        w.destroy()

    t = Thread(target = ipmain)
    t.start()

    w.mainloop()



