#!/usr/bin/python

__author__    = "Andrei Mikhailov"
__copyright__ = "Copyright 2013, Andrei Mikhailov"
__license__   = "GPL"

import sqlite3
import re
import Tkinter
import tkFont
import sys
import os
import itertools
import functools
import yaml
import Pmw
from inspect import currentframe,getargvalues
import IPython.utils as utils

class Aux(object):
    """this class determines various settings:"""
    class AuxConf(object):
        max_buttons=12
        font1 = ("dejavu sans mono", 15)
        font2 = ("dejavu sans mono", 12)
        font1Bold = ("dejavu sans mono", 15, tkFont.BOLD)
        font2Bold = ("dejavu sans mono", 12, tkFont.BOLD)
        textfield_width = 32
        textfield_bg_color = 'white'
        slider_fg_color = 'lightblue'
        slider_throughcolor = '#FF9966'
        slider_active_bg_color = 'coral1'
        slider_width_int = 8
        commandline_width = 44
        expand_ver = True
        expand_hor = True
        canvas_update_color = "#ff7f00"
        canvas_collect_color = "#009900"
        #field_colors = ["#99ccff", "#ffcc99", "#99ff99", "#ff99ff" ]
        field_colors = ["#FFAAAA", "#ffccaa", "#ffffaa", "#aaffaa", "#aaccff", "#bbaaff", "#ddaaff"]
        button_color1 = "#eeeeaa"
        default_max_field_width = 12
        max_but_text_width = 20
        starter_frame_highlight = "red"
        starter_frame_bg = "#FFEFD5"
        starter_frame_bg_bottom = "#F0FFF0"
        starter_button_color = "lightblue"
        buttons_frame_bg = "#FFEFD5"
        buttons_buttons_bg = "lightblue"
        has_balloon_fg = "#AA2222"
    cnf = AuxConf()

    key_prefix_for_focus = "F" # will use F1 till F12 for moving in the form
    clr_normal    = utils.coloransi.TermColors().Normal
    clr_error     = utils.coloransi.TermColors().LightRed
    clr_show_sep1 = utils.coloransi.TermColors().LightRed
    clr_show_sep2 = utils.coloransi.TermColors().LightBlue
    bold = "\033[1m"
    reset = "\033[0;0m"

    ### No obvious settings below this line ############################
    dbfile='___FORGOT_TO_SET_DATABASE_FILE___'

    def unicode_truncate(self, s, length, encoding='utf-8'):
        # http://stackoverflow.com/questions/1809531/truncating-unicode-so-it-fits-a-maximum-size-when-encoded-for-wire-transfer
        encoded = s.encode(encoding)
        if len(encoded) > length:
            encoded = encoded.decode(encoding, 'ignore')[:length] + u'\u2026'
        return encoded

    def updateSel(self,x,sb):
        if not sb: return None
        if not Tkinter.SEL_FIRST: return None
        if x.compare(x.index(Tkinter.INSERT),">=",x.index(Tkinter.SEL_FIRST)):
        # Don't try to change ">=" to ">" here! Would get infinite loop!
            x.mark_set(Tkinter.SEL_LAST, x.index(Tkinter.INSERT))
        else:
            x.mark_set(Tkinter.SEL_FIRST, x.index(Tkinter.INSERT))
        x.tag_add(Tkinter.SEL, Tkinter.SEL_FIRST, Tkinter.SEL_LAST)

    def emacsify(self, tkinter_text):
        sep_re = " |\n|,|\\." ; nosep_re = "[^ \n,.]"
        sbool = [False]  # need an array to get it mutable
        def altF(e):
            tkinter_text.mark_set(Tkinter.INSERT,
                       tkinter_text.search(sep_re, tkinter_text.search(nosep_re,
                                     tkinter_text.index(Tkinter.INSERT),
                                     regexp=True ),
                                regexp=True)
                       )
            self.updateSel(tkinter_text, sbool[0])
        tkinter_text.bind("<Alt-f>",altF)
        def altB(e):
            tkinter_text.mark_set(Tkinter.INSERT,
                       tkinter_text.search(sep_re, tkinter_text.search(nosep_re,
                                     tkinter_text.index(Tkinter.INSERT), backwards=True,
                                     regexp=True ), backwards=True,
                                regexp=True) + " + 1 chars"
                       )
            if ( tkinter_text.index(Tkinter.INSERT) == tkinter_text.index(tkinter_text.index(Tkinter.END) + " - 1 chars" )) :
                tkinter_text.mark_set(Tkinter.INSERT, "1.0")
            self.updateSel(tkinter_text, sbool[0])
        tkinter_text.bind("<Alt-b>",altB)
        def altD(e):
            endpoint_ = tkinter_text.search(sep_re,
                                tkinter_text.search(nosep_re,
                                         tkinter_text.index(Tkinter.INSERT),
                                         regexp=True,
                                         stopindex=tkinter_text.index(Tkinter.END) ),
                                regexp=True, stopindex=tkinter_text.index(Tkinter.END))
            tkinter_text.clipboard_clear()
            tkinter_text.clipboard_append(tkinter_text.get(Tkinter.INSERT, endpoint_))
            tkinter_text.delete(tkinter_text.index(Tkinter.INSERT), endpoint_)
        tkinter_text.bind("<Alt-d>",altD)
        def altBS(e):
            pt1 = tkinter_text.search(nosep_re, tkinter_text.index(Tkinter.INSERT),
                           regexp=True, backwards=True, stopindex="0.0" )
            if pt1 == "" : pt1 = "0.0"
            startpoint_ = tkinter_text.search(sep_re, pt1, regexp=True, backwards=True,
                                  stopindex="0.0")
            if startpoint_ == "" : startpoint_ = "0.0"
            else : startpoint_ = startpoint_ + " + 1 chars "
            tkinter_text.clipboard_clear()
            tkinter_text.clipboard_append(tkinter_text.get(startpoint_ , Tkinter.INSERT))
            tkinter_text.delete(startpoint_, Tkinter.INSERT)
            return "break"
        tkinter_text.bind("<Alt-BackSpace>",altBS)
        def controlSpace(e):
            if sbool[0]:
                tkinter_text.tag_delete("liniitag")
                tkinter_text.tag_add(Tkinter.SEL, Tkinter.SEL_FIRST, Tkinter.SEL_LAST)
            else:
                tkinter_text.mark_set(Tkinter.SEL_FIRST, tkinter_text.index(Tkinter.INSERT))
                tkinter_text.mark_set(Tkinter.SEL_LAST, tkinter_text.index(Tkinter.INSERT))
                tkinter_text.tag_add("liniitag", tkinter_text.index(Tkinter.INSERT))
            sbool[0] = not(sbool[0])
        tkinter_text.bind("<Control-space>",controlSpace)
        def controlG(e):
            try:
                tkinter_text.tag_remove(Tkinter.SEL, Tkinter.SEL_FIRST, Tkinter.SEL_LAST)
                tkinter_text.tag_delete(Tkinter.SEL)
                tkinter_text.mark_unset(Tkinter.SEL_FIRST); tkinter_text.mark_unset(Tkinter.SEL_LAST)
            except Tkinter.TclError as e:
                pass
            tkinter_text.tag_delete("liniitag")
            sbool[0] = False
        tkinter_text.bind("<Control-g>",controlG)
        def controlF(e):
            if sbool[0]:
                tkinter_text.mark_set(Tkinter.INSERT, tkinter_text.index(Tkinter.INSERT) + " + 1 chars")
                self.updateSel(tkinter_text, sbool[0])
                return "break"
                # we are returning break to prevent further processing
        tkinter_text.bind("<Control-f>",controlF)
        def controlB(e):
            if sbool[0]:
                tkinter_text.mark_set(Tkinter.INSERT, tkinter_text.index(Tkinter.INSERT) + " - 1 chars")
                self.updateSel(tkinter_text, sbool[0])
                return "break"
                # we are returning break to prevent further processing
        tkinter_text.bind("<Control-b>",controlB)
        def controlW(e):
            sbool[0] = False
        tkinter_text.bind("<Control-w>",controlW)
        def controlV(e):
            xheightuple = tkinter_text.config()["height"]
            xh = xheightuple[ len(xheightuple) - 1 ]
            tkinter_text.yview_scroll(xh/2 + 1, "units")
            tkinter_text.mark_set(Tkinter.INSERT,
                       tkinter_text.index(Tkinter.INSERT) + " + " + str(xh/2 + 1) + " lines")
        tkinter_text.bind("<Control-v>",controlV)
        def altV(e):
            xheightuple = tkinter_text.config()["height"]
            xh = xheightuple[ len(xheightuple) - 1 ]
            tkinter_text.yview_scroll( - ( xh/2 + 1 ), "units")
            tkinter_text.mark_set(Tkinter.INSERT,
                       tkinter_text.index(Tkinter.INSERT) + " - " + str(xh/2 + 1) + " lines")
        tkinter_text.bind("<Alt-v>",altV)
        
    def textfield(self, parent_frame, **settings):
        """The main entryfield with Emacs bindings"""
        x = Tkinter.Text(parent_frame, font = self.cnf.font1,
                         bg=self.cnf.textfield_bg_color,
                         width = self.cnf.textfield_width,
                         highlightthickness=0, relief=Tkinter.FLAT,
                         state=Tkinter.NORMAL, **settings)
        # I will later use Control-Return to collect data, so I need to unbind it:
        x.bind_class("Text", "<Control-Return>", lambda e: None)
        # alternative way is found here:
        # http://www.pythonware.com/library/tkinter/introduction/events-and-bindings.htm
        # http://stackoverflow.com/questions/3215549/tkinter-set-cursor-position
        # http://effbot.org/tkinterbook/text.htm
        x.tag_config("liniitag", underline=True)
        # manual on REGEX: http://tmml.sourceforge.net/doc/tcl/re_syntax.html
        self.emacsify(x)
        return x

    def label(self, parent_frame, **settings):
        x = Tkinter.Label(parent_frame, font = self.cnf.font2, padx=0,
                          borderwidth=0, highlightthickness=0, **settings)
        return x

    def button(self, parent_frame, **settings):
        x = Tkinter.Button(parent_frame, font = self.cnf.font2, **settings)
        return x

    def checkbutton(self, parent_frame, **settings):
        x = Tkinter.Checkbutton(parent_frame, font = self.cnf.font2,
                                borderwidth=0, highlightthickness=0, **settings)
        return x

    def button1(self, parent_frame, **settings):
        x = Tkinter.Button(parent_frame, font = self.cnf.font2, highlightthickness = 2,
                           highlightbackground = self.cnf.button_color1, borderwidth = 0,
                           bg = self.cnf.button_color1, relief = Tkinter.FLAT, **settings)
        return x

    def button_navig(self, parent_frame, **settings):
        x = Tkinter.Button(parent_frame, font = self.cnf.font2, fg="#80c0e0", bg="#a05050",
                           activebackground = "#77ff77",
                           **settings)
        return x

    def button_red(self, parent_frame, **settings):
        x = Tkinter.Button(parent_frame, font = self.cnf.font2, fg="#ffffff", bg="#ff0000",
                           **settings)
        return x

    def button_blue(self, parent_frame, **settings):
        x = Tkinter.Button(parent_frame, font = self.cnf.font2, fg="#ffffff", bg="#0000ff",
                           **settings)
        return x

    def textline(self, parent_frame, **settings):
        x = Tkinter.Entry(parent_frame, font = self.cnf.font1,
                          width = self.cnf.commandline_width, **settings)
        return x

    def hline(self, frame, color):
        Tkinter.Canvas(frame, bg=color, height='1p', highlightthickness=0).pack(fill=Tkinter.X)

    def columns(self, tname):
        answer=[]
        for a in ex("pragma table_info("+tname+")"):
            answer.append(a[1])
        return answer

    def prnt(self,x):
        try:
            print(x)
        except UnicodeEncodeError :
            print("************************* UNICODE  ERROR ************************")
            print("*                                                               *")
            print(x.encode('ascii', 'replace'))
            print("*                                                               *")
            print("************************* UNICODE  ERROR ************************")

    class _DialogOutline(object):
        def __init__(self, specs, title, canvas_color, auxObj):
            self.mainwin = Tkinter.Tk()
            self.mainwin.title(title)
            self.auxObj = auxObj
            self.tblname = specs['tablename']
            self.balloons  = specs['balloons'] if 'balloons' in specs.keys() else None
            self.color = canvas_color
            self.txtfield = {}  # this is a dictionary, it will be set up in my.insert_textfields()
                                # we will later ``attach'' more fields to DialogOutline
            self.columnspecs = filter(lambda x:x[3], specs['columns'])
            frame_out = Tkinter.Frame(self.mainwin, bg = self.color)
            fill_dir = Tkinter.NONE
            if auxObj.cnf.expand_hor : fill_dir = Tkinter.X
            if auxObj.cnf.expand_ver : fill_dir = Tkinter.Y
            if (auxObj.cnf.expand_ver and auxObj.cnf.expand_hor) : fill_dir = Tkinter.BOTH
            frame_out.pack(fill = fill_dir, expand = auxObj.cnf.expand_ver or auxObj.cnf.expand_hor)
            x = Tkinter.Frame(frame_out)
            x.pack(padx=3, fill = fill_dir, expand = auxObj.cnf.expand_ver or auxObj.cnf.expand_hor)
            self.frame=x
        def insert_textfields(self, prefill, locked=False):
            ao = self.auxObj
            ao.hline(self.frame, self.color)
            ao.hline(self.frame, self.color)
            frame_grid = Tkinter.Frame(self.frame)
            frame_grid.grid_columnconfigure(1, weight = 1 if ao.cnf.expand_hor else 0)
            j=0
            jmax = len(self.columnspecs) - 1
            for itm in self.columnspecs:
                i0=itm[0]; i1=itm[1]; i2=itm[2]
                lab_left = ao.label(frame_grid, text=i1+" "+ (str(j+1) if j<12 else "C"+str(j-11))  )
                lab_right= ao.label(frame_grid, text= "-> "+i0)
                if self.balloons and i0 in self.balloons.keys():
                    lab_left.config(fg = ao.cnf.has_balloon_fg, font = ao.cnf.font2Bold)
                    balloon1 = Pmw.Balloon(frame_grid)
                    balloon1.bind(lab_left, str(self.balloons[i0]))
                if prefill[i0]:
                    balloon2 = Pmw.Balloon(frame_grid)
                    balloon2.bind(lab_right, prefill[i0])
                slider_frame = Tkinter.Frame(frame_grid)
                indicator_label = Tkinter.Label(slider_frame, text='')
                indicator_label.pack(side=Tkinter.RIGHT, fill=Tkinter.Y)
                # The following is to allow the text field to expand vertically:
                if i2>1 : frame_grid.grid_rowconfigure(2*j, weight= 1 if ao.cnf.expand_ver else 0)
                self.txtfield[itm] = ao.textfield(slider_frame, height=i2)
                # On the very first item I want to calculate the size in pt of the textfield
                # width which was specified in monospace characters:
                if j==0 :
                    fnt4width = tkFont.Font(self.txtfield[itm],
                                      family= ao.cnf.font1[0], size= ao.cnf.font1[1])
                    minwidth = ( int( fnt4width.measure((ao.cnf.textfield_width + 1) * "x") )
                      + ao.cnf.slider_width_int + 1 )
                    frame_grid.grid_columnconfigure(1, minsize=minwidth)
                def focuser(txtfld):
                    return lambda event : txtfld.focus_set()
                if j < 12:
                    self.mainwin.bind("<" + ao.key_prefix_for_focus + str(j+1)+">",
                                      focuser(self.txtfield[itm]))
                else:
                    self.mainwin.unbind_all("<Control-Key-" + ao.key_prefix_for_focus + str(j-11)+">")
                    self.mainwin.bind("<Control-Key-" + ao.key_prefix_for_focus + str(j-11)+">",
                                      focuser(self.txtfield[itm]))
                if prefill[i0]:
                    self.txtfield[itm].insert(1.0, prefill[i0])
                slider = Tkinter.Scrollbar(slider_frame,
                                    width = str(ao.cnf.slider_width_int) + 'p', bg = ao.cnf.slider_fg_color,
                                    activebackground = ao.cnf.slider_active_bg_color,
                                    troughcolor = ao.cnf.slider_throughcolor
                                           )
                self.txtfield[itm].config(yscrollcommand=slider.set)
                slider.config(command=self.txtfield[itm].yview)
                # that was the standard scrollbar configuration rite:
                # http://effbot.org/zone/tkinter-scrollbar-patterns.htm
                lines_of_text = (self.txtfield[itm].get("1.0",Tkinter.END)[:-1]).split("\n")
                # num_of_wrapped_lines = sum([len(ln)>ao.cnf.textfield_width  for ln in lines_of_text])
                # if num_of_wrapped_lines > 0:
                #   indicator_label.config(bg = ao.line_wrap_warning_color)
                ### (This would not work because dont know the adjusted size...)
                v_size_of_text = sum([ int(len(ln)/(1+ao.cnf.textfield_width)) + 1 for ln in lines_of_text])
                if v_size_of_text > i2 :
                    slider.pack(side=Tkinter.LEFT,fill=Tkinter.Y)
                # want to pack slider before textfield, because dont want it to shrink away when resizing down
                self.txtfield[itm].pack(side=Tkinter.RIGHT, fill=Tkinter.BOTH, expand = ao.cnf.expand_ver or ao.cnf.expand_hor)
                slider_frame.grid(column=1, row=2*j, sticky=Tkinter.N + Tkinter.S + Tkinter.W + Tkinter.E)
                lab_left.grid(column=0, row=2*j, sticky=Tkinter.NE)
                lab_right.grid(column=2, row=2*j, sticky=Tkinter.NW)
                Tkinter.Canvas(frame_grid, bg=self.color, height='1p',
                               highlightthickness=0).grid(column=0, columnspan=3,
                                                          sticky=Tkinter.E+Tkinter.W, row=2*j+1)
                markedline = Tkinter.Canvas(frame_grid, bg=self.color, height='1p',
                               highlightthickness=0)
                if j < jmax:
                    markedline.create_line(minwidth - 6, 0, minwidth - 4, 0, fill=ao.cnf.textfield_bg_color)
                    markedline.create_line(minwidth - 6, 1, minwidth - 4, 1, fill=ao.cnf.textfield_bg_color)
                markedline.grid(column=1, columnspan=2, sticky=Tkinter.E+Tkinter.W, row=2*j+1)
                # locking the text field if needed:
                if locked:
                    self.txtfield[itm].config(state=Tkinter.DISABLED)
                j=j+1
            frame_grid.pack(fill=Tkinter.BOTH, expand = ao.cnf.expand_ver or ao.cnf.expand_hor)

        
    def DialogOutline(self, specs, title, canvas_color, auxObj):
        return self._DialogOutline(specs, title, canvas_color, auxObj)

    class _Buttons:
        def __init__(self, my, specs, criterium, show_cols,
                     max_field_width = None,
                     start_with=0, mainwin_but=None, highltd_flds = None):
            if not max_field_width: max_field_width = my.cnf.default_max_field_width
            if highltd_flds is None: highltd_flds = []
            # see here: http://effbot.org/zone/default-values.htm
            initframe = currentframe()
            arglist, _, _, _ = getargvalues(initframe)
            vardict = dict(zip(arglist[1:],
                               [locals()[x] for x in arglist[1:]]
                               )
                           )
            self.__dict__.update(vardict)
            del arglist, vardict
            ### This is instead of:
            # self.my = my ; self.specs = specs; self.criterium = criterium;
            # self.show_cols = show_cols ; self.start_with = start_with;
            # self.mainwin_but = mainwin_but;
            # self.highltd_flds = highltd_flds;
            # self.max_field_width = max_field_width;

            # print highltd_flds

            tblname=self.specs['tablename']; columnspecs=self.specs['columns'];
            balloons = self.specs['balloons'] if 'balloons' in self.specs.keys() else None
            if self.show_cols == "":
                show_in_first_row = []
                show_in_rows = columnspecs[0][0]
            elif self.show_cols == "*":
                show_in_first_row = [x[0] for x in columnspecs[1:]]
                show_in_rows = ','.join([x[0] for x in columnspecs])
            else:
                show_in_first_row = self.show_cols.split(",")
                show_in_rows = columnspecs[0][0] + "," + self.show_cols
            where_qmarks = " AND ".join([ "eqli(" + z[0] + ",?)"
                                          for z in columnspecs ])
            # where_qmarks = reduce(lambda x,y: x+" = ? AND "+y,
            #                       map(lambda z:z[0], columnspecs))+" = ? "
            ordered_cols = ','.join([z[0] for z in columnspecs])
        # starting GUI:
            if not(self.mainwin_but):
                self.mainwin_but=Tkinter.Tk()
                self.mainwin_but.title(self.specs['tablename']+": "+self.criterium)
                self.mainwin_but.config(bg = my.cnf.starter_frame_bg_bottom)
            name_of_mainwin_but = str(mainwin_but)
        # this modifier below is needed in order not to confuse bindings, when combining
        # several frames into one window; e.g. the <Fn> buttons are actually bound to mainwin_but
        # (not to individual buttons)
        # When we are focused on the child widget, we should search for the bindings of mainwin_but
        # (It does not happen automatically, thats why we need bindtags)
            def use_mainwin_bindings(w):
                oldbindtags = w.bindtags()
                w.bindtags(oldbindtags[:2] + (name_of_mainwin_but,) + oldbindtags[2:])
                return w
         ## top section:
            top_frame = Tkinter.Frame(self.mainwin_but)
            top_frame.config(bg = my.cnf.starter_frame_bg_bottom)
            top_frame.pack(fill = Tkinter.X)
            def exit_fn(event = None):
                self.delwin()
            self.mainwin_but.bind("<q>", exit_fn)
            topexit = my.button(top_frame,text="EXIT<q>",
                                bg = my.cnf.buttons_buttons_bg,
                                command = exit_fn )
            use_mainwin_bindings(topexit)
            topexit.pack(fill=Tkinter.X,side=Tkinter.RIGHT)
            Tkinter.Canvas(self.mainwin_but,bg="#000000",height='2p',
                           highlightthickness=0).pack(fill=Tkinter.X)
         ## middle section:
            def update_fn_buttons(mainwin_but, where_qmarks, all_in_row,
                                  specs, criterium, show_cols, start_with):
                def returned_fn(dummy_for_event = None):
                    my.prnt("about to UPDATE "+specs['tablename']+" where "+where_qmarks+\
                            "\n(?,...,?) ---> "+str(tuple(all_in_row)))
                    update(specs, "where "+where_qmarks, tuple(all_in_row), self,
                           readonly = True)
                    return "break"
                return returned_fn

            middle_frame = Tkinter.Frame(self.mainwin_but)
            middle_frame.config(bg = my.cnf.starter_frame_bg_bottom)
            middle_frame.pack(fill = Tkinter.X)
        # prepare and show buttons:
            itm_but={}; itm_frame={}
            but_iter_1 = itertools.izip(
                ex("select " + show_in_rows + " from "+tblname+" "+self.criterium),
                ex("select " + ordered_cols + " from "+tblname+" "+self.criterium)
                )
            but_iter_lst = list(but_iter_1)
            self.number_of_items = len(but_iter_lst)
            but_iter = iter(but_iter_lst)
            # note that I cloned the iterator,
            # to avoid the lockdown of database, which started appearing
            # after I added wait_window()
            j=0
            self.clmn_of_lbls = dict([(p,[]) for p in range(0,len(columnspecs)+1)])
            def change_color_fn(k, btn):
                def f(e = None):
                    if (k in self.highltd_flds) :
                        self.highltd_flds.remove(k)
                        for lbl in self.clmn_of_lbls[k] :
                            lbl.config(bg = my.cnf.field_colors[k % len(my.cnf.field_colors)])
                            btn.config(bg = my.cnf.field_colors[k % len(my.cnf.field_colors)])
                    else:
                        self.highltd_flds.append(k)
                        for lbl in self.clmn_of_lbls[k] :
                            lbl.config(bg = 'white')
                            btn.config(bg = 'white')
                return f
            for shown_in_row, all_in_row in itertools.islice(but_iter, self.start_with,
                                                             self.start_with + my.cnf.max_buttons):
                if shown_in_row:
                    itm_frame[all_in_row]=Tkinter.Frame(middle_frame)
                    itm_frame[all_in_row].config(bg = my.cnf.starter_frame_bg_bottom)
                    itm_frame[all_in_row].pack(fill=Tkinter.X)
                    command_push_but = update_fn_buttons(self.mainwin_but, where_qmarks,
                                                        all_in_row,
                                                        self.specs, self.criterium, self.show_cols, self.start_with)
                    self.mainwin_but.bind("<" + my.key_prefix_for_focus + str(j+1)+">",
                                     command_push_but)
                    fnumber = str(j+1) if j > 8 else " "+str(j+1)
                    itm_but[all_in_row] = my.button1(
                              itm_frame[all_in_row],
                              text=fnumber + ". " + my.unicode_truncate(str(shown_in_row[0]),
                                                                        my.cnf.max_but_text_width),
                              command=command_push_but )
                    use_mainwin_bindings(itm_but[all_in_row])
                    Tkinter.Label(itm_frame[all_in_row], text='', borderwidth=0,
                                  padx=1, bg='black').pack(side=Tkinter.LEFT,fill=Tkinter.Y)
                    itm_but[all_in_row].pack(side=Tkinter.LEFT,fill=Tkinter.X)
                    Tkinter.Label(itm_frame[all_in_row], text='', borderwidth=0,
                                  padx=1, bg='black').pack(side=Tkinter.LEFT,fill=Tkinter.Y)
                    if len(shown_in_row) > 1:
                        clr_num = 0
                        for fld in shown_in_row[1:]:
                            lbl_txt = str(fld).split("\n")[0]
                            lbl_txt = my.unicode_truncate(lbl_txt, self.max_field_width)
                            lbl_txt = " "+lbl_txt+" "
                            w = my.label(itm_frame[all_in_row], text=lbl_txt)
                            if clr_num in self.highltd_flds:
                                w.config(bg = 'white')
                            else:
                                w.config(bg = my.cnf.field_colors[clr_num % len(my.cnf.field_colors)])
                            ((self.clmn_of_lbls)[clr_num]).append(w)
                            balloon = Pmw.Balloon(itm_frame[all_in_row])
                            balloon.bind(w, str(fld))
                            w.pack(side=Tkinter.LEFT, fill=Tkinter.Y)
                            clr_num = clr_num + 1
                    j = j+1
                else: break
                Tkinter.Canvas(middle_frame,bg="#000000", height='1p',
                               highlightthickness=0).pack(fill=Tkinter.X)
         ## bottom section:
            Tkinter.Canvas(self.mainwin_but,bg="#000000",height='1p',
                           highlightthickness=0).pack(fill=Tkinter.X)
            bot_frame = Tkinter.Frame(self.mainwin_but)
            bot_frame.config(bg = my.cnf.starter_frame_bg_bottom)
            bot_frame.pack(fill = Tkinter.X)
        # preparing the ``NEW'' and ``RELOAD'' buttons:
            def new_fn(event = None):
                collect(specs, self)
                return "break"
            def reload_fn(dummy_for_event = None):
                self.clearwin()
                self.rebuild()
                return "break"
            self.mainwin_but.bind("<n>", new_fn)
            def detach_fn(ev = None):
                self.delwin()
                self.mainwin_but=Tkinter.Tk()
                self.mainwin_but.title(self.specs['tablename']+": "+self.criterium)
                self.rebuild()
            self.mainwin_but.bind("<d>", detach_fn)
            newbutton = my.button(bot_frame, bg = my.cnf.buttons_buttons_bg,
                                  text = "NEW<n>", command = new_fn)
            use_mainwin_bindings(newbutton)
            newbutton.pack(fill=Tkinter.X,side=Tkinter.LEFT)
            self.mainwin_but.bind("<r>", reload_fn)
            reloadbutton = my.button(bot_frame, bg = my.cnf.buttons_buttons_bg,
                                     text = "RELOAD<r>", command = reload_fn)
            use_mainwin_bindings(reloadbutton)
            reloadbutton.pack(fill=Tkinter.X,side=Tkinter.LEFT)
            detachbutton = my.button(bot_frame, bg = my.cnf.buttons_buttons_bg,
                                     text = "DETACH<d>", command = detach_fn)
            use_mainwin_bindings(detachbutton)
            detachbutton.pack(fill=Tkinter.X,side=Tkinter.LEFT)
        # figure out if there are more items:
            try:
                self.there_are_more_items = but_iter.next()
            except StopIteration:
                self.there_are_more_items = False
        # preparing the ``go back'' button, if started not from the first row:
            if self.start_with > 0:
                def back_fn(dummy_for_event = None):
                    self.clearwin()
                    if self.start_with > my.cnf.max_buttons - 1 :
                        self.start_with = self.start_with - my.cnf.max_buttons ;
                    self.rebuild()
                self.mainwin_but.bind("<b>", back_fn)
                self.mainwin_but.bind("<h>", back_fn)
                forwardbutton = my.button_navig(top_frame, text="<-",
                                              command= back_fn)
                use_mainwin_bindings(forwardbutton)
                forwardbutton.pack(padx=0,side=Tkinter.LEFT)
            else:
                if self.there_are_more_items :
                    my.button(top_frame, text="--").pack(padx=0,side=Tkinter.LEFT)
        # preparing the ``go forward'' button, if there are more items than shown:
            if self.there_are_more_items:
                def forward_fn(dummy_for_event = None):
                    self.clearwin()
                    if (self.start_with + my.cnf.max_buttons < self.number_of_items):
                        self.start_with = self.start_with + my.cnf.max_buttons ;
                    self.rebuild()
                self.mainwin_but.bind("<f>", forward_fn)
                self.mainwin_but.bind("<l>", forward_fn)
                backbutton = my.button_navig(top_frame, text="->",
                                             command=forward_fn)
                use_mainwin_bindings(backbutton)
                backbutton.pack(padx=0,side=Tkinter.LEFT)
            clr_num = 0;
            for cl in show_in_first_row:
                fbtn = my.button(top_frame, text=str(cl))
                use_mainwin_bindings(fbtn)
                # configure popup:
                if balloons:
                    if cl in balloons.keys():
                        fbtn.config(font = my.cnf.font2Bold)
                        balloon = Pmw.Balloon(top_frame)
                        balloon.bind(fbtn, str(balloons[cl]))
                if clr_num in self.highltd_flds :
                    fbtn.config(bg = 'white')
                else :
                    fbtn.config(bg = my.cnf.field_colors[clr_num % len(my.cnf.field_colors)])
                fbtn.config(command = change_color_fn(clr_num,fbtn))
                fbtn.pack( side = Tkinter.LEFT, fill = Tkinter.Y )
                clr_num = clr_num + 1
            totalnum_lbl = my.label(top_frame, bg = my.cnf.starter_frame_bg_bottom,
                                    text = "  " + str(1 + self.start_with) + "/" + str(self.number_of_items) )
            totalnum_lbl.pack( side = Tkinter.LEFT, fill = Tkinter.Y )
        # the lower exit button:
            lowerexitbutton = my.button(bot_frame, bg = my.cnf.buttons_buttons_bg,
                                        text="EXIT<q>",
                                        command = exit_fn )
            use_mainwin_bindings(lowerexitbutton)
            lowerexitbutton.pack(fill=Tkinter.X,side=Tkinter.RIGHT)
            # added on Mon Nov 23 23:59:30 JST 2009:
            self.mainwin_but.wait_window()
        def clearwin(self):
            if self.mainwin_but :
                for subw in self.mainwin_but.winfo_children():
                    subw.destroy()
        def delwin(self):
            if self.mainwin_but :
                self.mainwin_but.destroy()
            else: print "mainwin_but already destroyed"
        def rebuild(self):
            self.my.ConstructButtons(self.specs, self.criterium, self.show_cols, self.max_field_width,
                             self.start_with, self.mainwin_but, self.highltd_flds)

    def ConstructButtons(self, specs, criterium, show_cols,
                         max_field_width = None,
                         start_with = 0, mainwin_but = None,
                         highltd_flds = None):
        if not max_field_width: max_field_width = my.cnf.default_max_field_width
        if highltd_flds is None: highltd_flds = []
        # see here: http://effbot.org/zone/default-values.htm
        return self._Buttons(self, # here self is my
                             specs, criterium, show_cols, max_field_width,
                             start_with, mainwin_but, highltd_flds,
                             )

    def eqsql(self, x, y):
        return x == y

    def hastags(self, x, y):
        """Checks if y matches the tag pattern x,
            an example of a tag pattern is 'aaa,bbb|^ccc' """
        xs  = x.split("|")
        xss = map(lambda u: u.split(",") , xs)
        ys  = y.split(",")
        def myMatch1S(pat,strs):
            if pat[0] == "^" :
                return functools.reduce(
                    lambda p,q : ( p and q ) ,
                    map(lambda str_ : pat[1:] != str_ , strs),
                    True)
            else :
                return functools.reduce(
                    lambda p,q : ( p or q ) ,
                    map(lambda str_ : pat == str_ , strs),
                    False)
        def myMatchSS(pats,strs):
            return functools.reduce(
                lambda p,q : (p and q) ,
                map(lambda pat: myMatch1S(pat,strs) , pats),
                True)
        answer = functools.reduce(
                lambda p,q : (p or q) ,
                map(lambda u : myMatchSS(u,ys) , xss) ,
                False)
        if (answer) : return 1
        else : return 0

    def getlink(self):
        link = sqlite3.connect(self.dbfile)
        link.create_function("tags",2,self.hastags)
        link.create_function("eqli",2,self.eqsql)
        return link

    alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
                'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
                'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

    class Getch: #use: inpt = my.Getch(my)()
        """Gets a single character from standard input.  Doesn't echo to screen."""
        def __init__(self,my):
            try:
                self.impl = my._GetchWindows()
            except ImportError:
                self.impl = my._GetchUnix()

        def __call__(self):
            return self.impl()

    class _GetchUnix:
        def __init__(self):
            import tty, sys

        def __call__(self):
            import sys, tty, termios
            fd = sys.stdin.fileno()
            old_settings = termios.tcgetattr(fd)
            try:
                tty.setraw(sys.stdin.fileno())
                ch = sys.stdin.read(1)
            finally:
                termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
            return ch

    class _GetchWindows:
        def __init__(self):
            import msvcrt

        def __call__(self):
            import msvcrt
            return WindowsError("Andrei: Configure for Windows!")
            #return msvcrt.getch()

    class LiniiError(Exception):
        def __init__(self, value):
            self.value = value
        def __str__(self):
            return repr(self.value)


class Dataclass(): pass
data = Dataclass()
my = Aux()
config = my.cnf


def collect(specs, buttonsObj=None, **prefill):
    """linii.collect(table_descr) ,
       where table_descr was imported from the file databasename.py;
       where "nick" and "notes" are the names of the columns """
    ## This starts the GUI:
    inserted_values = []
    card = my.DialogOutline(specs, "collect "+specs['tablename'], my.cnf.canvas_collect_color, my)
    card.columnspecs = filter(lambda x:x[3], specs['columns'])
    # the fourth field in the 'columns' array is True/False specifying whether or not
    # to request the value at the collection time. The False is used if the value has
    # a default value which will be automatically filled at the time of the collection
    for t in [z[0] for z in card.columnspecs]:
        if not prefill.has_key(t): prefill[t]=''
    ordered_cols = ",".join(z[0] for z in card.columnspecs)
#    my.hline(card.frame, card.color)
    card.insert_textfields(prefill)
    def collect_fn(card, ordered_cols, buttonsObj, vs):
        def return_fn(dummy_for_event = None):
            values=();
            for itm in card.columnspecs:
                values=values+(card.txtfield[itm].get("1.0",Tkinter.END)[:-1],)
                        # here -1 is to get rid of \n
            qmarks='(' + '?,'*(len(values)-1) + '?)'
            my.prnt("about to INSERT into "+card.tblname+" ("\
                    +ordered_cols+") values "+qmarks+"\n(?,...?) ---> "+str(values))
            ex("insert into "+card.tblname+" ("+ordered_cols+") values "+qmarks, values).close()
            vs.append(values)
            # verify the insertion of the new data:
            # where_qmarks   = reduce(lambda x,y: x+" = ? AND "+y,
            #                         map(lambda z:z[0], card.columnspecs))  + " = ? "
            where_qmarks = " AND ".join([ "eqli(" + z[0] + ",?)"
                                          for z in card.columnspecs ])
            for a in ex("select * from "+card.tblname+" where "+where_qmarks,
                        tuple(values)):
                for y in zip(map(lambda u: "__________"+u+"_"*(32-len(u)) ,
                                 my.columns(card.tblname) ) ,
                             list(a)):
                    if y[1]:
                        print(y[0])
                        my.prnt(y[1])
                print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
            card.mainwin.destroy()
            if buttonsObj :
                buttonsObj.clearwin()
                buttonsObj.rebuild()
        return return_fn
    my.hline(card.frame, card.color)
    collect_button = my.button1(card.frame, text = 'collect',
                                command = collect_fn(card, ordered_cols,
                                                     buttonsObj, inserted_values) )
    collect_button.pack(fill=Tkinter.X)
    card.mainwin.bind("<Control-Return>", collect_fn(card, ordered_cols,
                                                     buttonsObj, inserted_values) )
    for _ in [1,2,3]: my.hline(card.frame, card.color)
    # added on Mon Nov 23 23:59:30 JST 2009:
    card.mainwin.wait_window()
    return inserted_values

def update(specs, criterium, data_tuple=None,
           buttonsObj = None, readonly = False):
    """linii.update(table_descr,"criterium") ,
       where table_descr was imported from the file databasename.py
       and criterium is something like "where nick='Andrei'" """
    newdata_qmarks = reduce(lambda x,y: x+" = ?,"+y,
                            map(lambda z:z[0], specs['columns']))  + " = ?"
    where_qmarks = " AND ".join([ "eqli(" + z[0] + ",?)"
                                      for z in specs['columns'] ])
    # where_qmarks   = reduce(lambda x,y: x+" = ? AND "+y,
    #                         map(lambda z:z[0], specs['columns']))  + " = ? "
    ordered_cols   = reduce(lambda x,y: x+","+y,
                            map(lambda z:z[0], specs['columns']))
    def exit_fn_update(card):
        def returned_fn(dummy_for_event = None):
            card.mainwin.destroy()
        return returned_fn
    def unlock_fn(card, specs, criterium, data_tuple, buttonsObj):
        def returned_fn(dummy_for_event = None):
            card.mainwin.destroy()
            update(specs, criterium, data_tuple, buttonsObj, readonly = False)
        return returned_fn
    def update_fn(card, newdata_qmarks, where_qmarks, selected_row,
                  buttonsObj):
        def returned_fn(dummy_for_event = None):
            # (here dummy_for_event is needed because I will also use
            #  update_fn in keyboard binding)
            link = my.getlink()
            new_data=map(lambda x: card.txtfield[x].get("1.0",Tkinter.END)[:-1],
                         card.columnspecs)
            # the -1 was to get rid of \n at the end of the last line of the text widget
            curs = link.cursor()
            curs.execute("update "+card.tblname+" set "+newdata_qmarks+" where "+where_qmarks,
                          tuple(new_data) + tuple(selected_row))
            link.commit()
            curs.close()
        # verify the insertion of the new data:
            for a in ex("select * from "+card.tblname+" where "+where_qmarks,
                        tuple(new_data)):
                for y in zip(map(lambda u: "__________"+u+"_"*(32-len(u)) ,
                                 my.columns(card.tblname) ) ,
                             list(a)):
                    if y[1]:
                        print(y[0])
                        my.prnt(y[1])
                print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
            card.mainwin.destroy()
            if buttonsObj :
                buttonsObj.clearwin()
                buttonsObj.rebuild()
        return returned_fn;
    def clone_fn(card, buttonsObj):
        def fn():
            new_data_dict = {}
            for f in card.columnspecs:
                new_data_dict[f[0]] = card.txtfield[f].get("1.0",Tkinter.END)[:-1]
            collect(specs, buttonsObj, **new_data_dict)
        return fn
    def delete_fn(del_frame, card, where_qmarks, selected_row,
                  buttonsObj):
        def returned_fn():
            confirm_frame=Tkinter.Frame(del_frame)
            confirm_label=my.label(confirm_frame,text="Are you sure?")
            def yes_fn():
                link=my.getlink()
                curs = link.cursor()
                my.prnt( "delete from "+card.tblname+" where "+where_qmarks\
                        +"\nWHERE DATA WAS "+str(tuple(selected_row)) )
                my.prnt( "--- we always try to delete only one row ---" )
                try:
                # First try to use the rowid trick to remove only one record,
                # as described here: http://stackoverflow.com/questions/2791006/sqlite-simple-delete-statement-did-not-work
                # This is needed in case if there are several identical rows.
                    curs.execute(" delete from " + card.tblname +
                                 " where rowid = ( select rowid from " + card.tblname +
                                 " where " + where_qmarks + " limit 1 ) ",
                                 tuple(selected_row))
                except sqlite3.Error as e:
                    print "|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|"
                    print "| FAILED TO REMOVE ONLY ONE RECORD BY USING rowid |"
                    print "|          THE TABLE DOES NOT HAVE rowid?         |"
                    print "|  proceeding bluntly, perhaps removing all rows  |"
                    print "|_________________________________________________|"
                    curs.execute("delete from " + card.tblname + " where " + where_qmarks,
                                 tuple(selected_row))
                link.commit()
                curs.close()
                card.mainwin.destroy()
                if buttonsObj :
                    buttonsObj.clearwin()
                    buttonsObj.rebuild()
            yes_button=my.button_red(confirm_frame, text='Yes', command=yes_fn)
            def no_fn():
                confirm_frame.destroy()
            no_button=my.button_blue(confirm_frame, text='No', command=no_fn)
            confirm_label.pack(side=Tkinter.LEFT)
            yes_button.pack(side=Tkinter.LEFT)
            no_button.pack(side=Tkinter.LEFT)
            confirm_frame.pack(fill=Tkinter.X)
        return returned_fn
    def unfocus_fn(card):
        def returned_fn(dummy_for_event = None):
            card.mainwin.focus_set()
        return returned_fn
    ## Iteration over the rows satisfying the specified criterium:
    rows_matching = list(ex("select "+ordered_cols+
                            " from "+specs['tablename']+" "+criterium,
                            data_tuple))
    for selected_row in iter(rows_matching):
        prefill=dict(zip(map(lambda z:z[0], specs['columns']), selected_row))
        ## This starts the GUI:
        card = my.DialogOutline(specs,
                                "update "+specs['tablename']+":"+str(prefill[specs['columns'][0][0]]),
                                my.cnf.canvas_update_color, 
                                my)
        card.columnspecs = specs['columns']
        for i in [1,2,3]: my.hline(card.frame, card.color)
        del_frame = Tkinter.Frame(card.frame)
        del_button = my.button(del_frame, text='delete',
                               command=delete_fn(del_frame, card,
                                                 where_qmarks, selected_row,
                                                 buttonsObj))
        del_frame.pack(fill=Tkinter.X)
        del_button.pack(side=Tkinter.LEFT)
        card.mainwin.bind("<Escape>",unfocus_fn(card))
        if readonly: 
            command_unlock = unlock_fn(card, specs, criterium,
                                       data_tuple, buttonsObj)
            card.mainwin.bind("<Control-grave>",command_unlock)    
            unlock_button = my.button(del_frame, text='unlock',
                                      command=command_unlock)
            unlock_button.pack(side=Tkinter.RIGHT, fill=Tkinter.X)
        clone_button = my.button(del_frame, text="clone",
                                 command=clone_fn(card, buttonsObj))
        clone_button.pack(side=Tkinter.RIGHT, fill=Tkinter.X)
        command_exit_button = exit_fn_update(card)
        exit_button = my.button(del_frame, text='exit',
                                command=command_exit_button)
        exit_button.pack(side=Tkinter.RIGHT, fill=Tkinter.X)
        card.insert_textfields(prefill, readonly)
        if not(readonly):
            my.hline(card.frame, card.color)
            update_button = my.button1(card.frame, text='update',
                                       command = update_fn(card,
                                                   newdata_qmarks, where_qmarks,
                                                   selected_row, buttonsObj))
            card.mainwin.bind("<Control-Return>", update_fn(card,
                                                   newdata_qmarks, where_qmarks,
                                                   selected_row, buttonsObj))
            update_button.pack(fill=Tkinter.X)
        for i in [1,2,3]: my.hline(card.frame, card.color)
        # added on Mon Nov 23 23:59:30 JST 2009:
        card.mainwin.wait_window()

def buttons(specs, criterium="", show_cols="*", maxwidth = 12, start_with=0, mainwin_but=None):
    """linii.buttons(table_descr, "criterium", "comma,separated,list,of,columns,to,show,besides,the,first,one", maxwidth),
       where table_descr was imported from the file databasename.py,
       criterium is something like "where nick='Andrei'",
       comma-separated-list-of-cols defaults to "*",
       maxwidth defaults to 12
       Use Ctrl-[ and Ctrl-] to move back and forward in the command history
       """
    my.ConstructButtons(specs, criterium, show_cols, maxwidth, start_with, mainwin_but)

def starter(specs, default_checked='', default_query=''):
    """
    linii.starter( table_descr, default_checked='', default_query='') --- a starter for linii.buttons
    To focus on the SQL command line press <c>
    To unfocus the SQL command line press <Escape>
    To jump between frames use <j> and <k>
    """
    mainwin_start=Tkinter.Tk()
    mainwin_start.title(specs['tablename']+": STARTER")
    mainwin_start.config(bg = my.cnf.starter_frame_bg_bottom)
    frame = Tkinter.Frame(mainwin_start)
    frame.config(bg = my.cnf.starter_frame_bg,
                 highlightcolor = my.cnf.starter_frame_highlight,
                 highlightthickness = 2)
    frame.pack(fill=Tkinter.X)
    name_of_mainwin = str(mainwin_start)
    def use_main_bindings(w):
        oldbindtags = w.bindtags()
        w.bindtags(oldbindtags[:2] + (name_of_mainwin,) + oldbindtags[2:])
        return w
    use_main_bindings(frame)
    frame_left = Tkinter.Frame(frame,bg = my.cnf.starter_frame_bg)
    frame_right = Tkinter.Frame(frame,bg = my.cnf.starter_frame_bg)
    frame_left.pack(side=Tkinter.LEFT,fill=Tkinter.Y)
    frame_right.pack(side=Tkinter.LEFT,fill=Tkinter.Y)
    cb_frame={}; cb={}; checked={}
    j=0
    mainwin_start.bind("<Escape>", lambda ev: frame.focus_set())
    for x in specs['columns']:
        j=j+1
        if 2*j - 2 < len(specs['columns']):
            cb_frame[x]=Tkinter.Frame(frame_left,bg = my.cnf.starter_frame_bg)
        else:
            cb_frame[x]=Tkinter.Frame(frame_right,bg = my.cnf.starter_frame_bg)
        cb_frame[x].pack(fill=Tkinter.X)
        cb[x] = use_main_bindings(
            my.checkbutton(cb_frame[x],
                           text= ("F" + str(j) if j<13 else "CF"+str(j-12)) + " " + x[0],
                           bg = my.cnf.starter_frame_bg,
                           activebackground = my.cnf.starter_frame_bg
                           )
            )
        checked[x]=Tkinter.IntVar(master=cb[x])
        cb[x].config(variable = checked[x])
        def toggle(ch):
            def ret_toggle(event):
                if ch.get(): ch.set(0)
                else: ch.set(1)
                return "break"
            return ret_toggle
        if j<13:
            mainwin_start.bind("<" + my.key_prefix_for_focus + str(j)+">", toggle(checked[x]))
        else:
            mainwin_start.unbind_all("<Control-Key-" + my.key_prefix_for_focus + str(j-12)+">")
            mainwin_start.bind("<Control-Key-" + my.key_prefix_for_focus + str(j-12)+">", toggle(checked[x]))
        if default_checked.split(",").count(x[0]):
            checked[x].set(1)
        cb[x].pack(side=Tkinter.LEFT)
    cmdline = use_main_bindings(my.textfield(frame, height = 4))
    mainwin_start.bind("<c>", lambda ev: cmdline.focus_set())
    def jump_to_next(ev):
        others = mainwin_start.winfo_children()
        now_fcs = mainwin_start.focus_get()
        if now_fcs and (now_fcs in others):
            if now_fcs == others[-1]: pass
            else :
                i = others.index(now_fcs)
                others[i+1].focus_set()
        return "break"
    def jump_to_prev(ev):
        others = mainwin_start.winfo_children()
        now_fcs = mainwin_start.focus_get()
        if now_fcs and (now_fcs in others):
            if now_fcs == others[0]: pass
            else :
                i = others.index(now_fcs)
                others[i-1].focus_set()
        return "break"
    mainwin_start.bind("<j>", jump_to_next)
    mainwin_start.bind("<k>", jump_to_prev)
    widthline = use_main_bindings(
        Tkinter.Entry(frame,  font=my.cnf.font1,
                      width = 3, borderwidth=0
                      )
        )
    widthline.insert(0,str(my.cnf.default_max_field_width))
    hist_back=[]
    hist_forw=[]
    def cmd_fn(checked, specs, cmdline, widthline):
        def returned_fn(event):
            checked_columns = filter(lambda x: checked[x].get(), specs['columns'])
            if len(checked_columns) == 0:
            # print("NO CHECKED")
                show_cols = ''
            else:
                show_cols = reduce(lambda x,y: x+","+y, map(lambda x: x[0], checked_columns))
            tmp_frame_for_buttons = Tkinter.Frame(mainwin_start)
            tmp_frame_for_buttons.config(highlightcolor = my.cnf.starter_frame_highlight,
                                         highlightthickness = 2)
            tmp_frame_for_buttons.pack(fill=Tkinter.X)
            tmp_frame_for_buttons.focus_set()
            cmdline_content = cmdline.get("1.0",Tkinter.END)[:-1]
            if hist_back and hist_back[-1] == cmdline_content: pass
            elif hist_forw and hist_forw[-1] == cmdline_content: pass
            else:
                hist_back.append(cmdline_content)
            # print hist_back
            # print hist_forw
            buttons(specs, cmdline_content, show_cols,
                    maxwidth = int(widthline.get()) ,
                    mainwin_but = tmp_frame_for_buttons)
        return returned_fn
    def hist_back_fn(event):
        current_content = cmdline.get("1.0",Tkinter.END)[:-1]
        h = current_content
        if hist_back:
            h = hist_back.pop()
            hist_forw.append(h)
        if h == current_content and hist_back:
            h = hist_back.pop()
            hist_forw.append(h)
        cmdline.delete(1.0,Tkinter.END)
        cmdline.insert(1.0, h)
    def hist_forw_fn(event):
        current_content = cmdline.get("1.0",Tkinter.END)[:-1]
        h = current_content
        if hist_forw:
            h = hist_forw.pop()
            hist_back.append(h)
        if h == current_content and hist_forw:
            h = hist_forw.pop()
            hist_back.append(h)
        cmdline.delete(1.0,Tkinter.END)
        cmdline.insert(1.0, h)
    cmdline.bind("<Control-bracketleft>", hist_back_fn)
    cmdline.bind("<Control-bracketright>", hist_forw_fn)
    cmdline.insert(1.0, default_query)
    cmdline.bind("<Control-Return>", cmd_fn(checked, specs, cmdline, widthline))
    # cmdline.bind("<KeyPress-Return>", cmd_fn(checked, specs, cmdline, widthline))
    cmdline.pack(fill=Tkinter.X)
    cmdline.focus_set()
    def exit_fn(mainwin_start):
        def return_fn():
            mainwin_start.destroy()
        return return_fn
    def new_fn(specs):
        def return_fn(event=None):
            if mainwin_start.focus_get() != cmdline:
                collect(specs)
            return "break"
        return return_fn
    def new_fn_for_mouse(specs):
        def return_fn(event=None):
            collect(specs)
            return "break"
        return return_fn
    my.button(frame, text="EXIT", bg=my.cnf.starter_button_color,
              command=exit_fn(mainwin_start)).pack(side=Tkinter.RIGHT)
    starter_new_but = my.button(frame, text="NEW<n>", bg=my.cnf.starter_button_color,
              command=new_fn_for_mouse(specs))
    starter_new_but.pack(side=Tkinter.LEFT)
    mainwin_start.bind("<n>", new_fn(specs))
    just_to_fill   = Tkinter.Label(frame,bg=my.cnf.starter_frame_bg, width=3)
    just_to_fill.pack(side=Tkinter.LEFT)
    maxwidth_label = my.label(frame,text="maxwidth:")
    maxwidth_label.config(bg=my.cnf.starter_frame_bg)
    maxwidth_label.pack(side=Tkinter.LEFT)
    widthline.pack(side=Tkinter.LEFT)
    # added on Mon Nov 23 23:59:30 JST 2009:
    mainwin_start.wait_window()

def null_repair(specs):
    """ null_repair( table_descr ) replaces NULL with '' everywhere in the table """
    for x in specs['columns']:
        print
        print("******* repairing "+x[0]+" **********")
        show(specs,"where "+x[0]+" is NULL ",specs['columns'][0][0])
        ex("update "+specs['tablename']+" set "+x[0]+" ='' where "+x[0]+" is NULL ")

def cursor():
    """ cursor() produces the database cursor with the row_factory"""
    link=my.getlink()
    link.row_factory = sqlite3.Row
    return link.cursor()

def ex(statement,x=None):
    """ ex("an SQL query") --- this is a "raw" sql command """
    link = my.getlink()
    curs = link.cursor()
    try:
        if x:
            answer = curs.execute(statement,x)
        else:
            answer = curs.execute(statement)
        link.commit()
        return answer
    except Exception as e:
        curs.close()
        link.close()
        print my.clr_error
        print e
        print my.clr_normal
        raise my.LiniiError("******* linii: error in ex() *******")
    curs.close()
    link.close()

def pragma(specs):
    """ pragma( table_descr ) prints out the table's pragma """
    for a in ex("pragma table_info("+specs['tablename']+")"):
        print(a)

def show(specs,query="",columns="*",values=None):
    """linii.show(table_descr,"where smth='something' order by nick","comma,sep,column,names"),
       the default value for "comma,sep,column,names" is "*" """
    tablename=specs['tablename']
    c = my.columns(tablename) if columns == "*" else columns.split(',')
    for a in ex("select " + columns + " from " + tablename + " " + query,values):
        j=1
        for b in zip(c,list(a)):
            if b[1] or j == 1:
                und = "-" if j == 1 else "-"
                color = my.clr_show_sep1 if j == 1 else my.clr_show_sep2
                print("   "+color+und*(4-2*j)+my.clr_normal+" "+my.bold+b[0]+my.reset+": "+
                       color+ und*(32-len(b[0]))*j + my.clr_normal)
                my.prnt(b[1]) ;  j=0

def lookup(specs,rexp,columns="*"):
    """linii.lookup(table_descr,regexp), linii.lookup(table_descr,regexp,"comma,sep,column,names"),
       where the default value of "comma,sep,column,names" is "*" """
    tablename=specs['tablename']
    p=re.compile(rexp)
    c = my.columns(tablename) if columns == "*" else columns.split(',')
    for a in ex("select " + columns + " from " + tablename):
        for b in list(a):
            if b:
                if p.search(str(b.encode('ascii','ignore'))):
                    j=1
                    for b1 in zip(c,list(a)):
                        if b1[1] or j == 1:
                            und = "-" if j == 1 else "-"
                            color = my.clr_show_sep1 if j == 1 else my.clr_show_sep2
                            print("   "+color+und*(4-2*j)+my.clr_normal+" "+my.bold+b1[0]+my.reset+": "+
                                   color+ und*(60-len(b1[0]))*j + my.clr_normal)
                            my.prnt(b1[1]) ;  j=0
                    break

def list_of_tables():
    list_of_tables = list(ex("""SELECT name FROM sqlite_master WHERE type = "table" """))
    return [x[0] for x in list_of_tables]

def read_yaml(yaml_filename = None):
    if not yaml_filename:
        yaml_filename, was_db = re.subn(r"\.db$", "", my.dbfile)
        if was_db:
            pass
        else:
            yaml_filename, was_sqlite = re.subn(r"\.sqlite$", "", my.dbfile)
        yaml_filename = yaml_filename + ".yaml"
    yamfl = open(yaml_filename, 'r')
    y = yaml.safe_load(yamfl)
    yamfl.close()
    my.dbfile = y['dbfile']
    z = y['data']
    # the yaml safe_load can only load list (?)
    # but I prefer column specs to be tuples
    # of the form (col_name, human_readable_name, how_many_lines, if_show_on collect)
    # therefore I need to transform lists into tuples:
    for u in z.keys():
        z[u]['columns'] = map( tuple, z[u]['columns'] )
    data.__dict__.update(z)

def setup_yaml(table_name):
    yaml_filename, was_db = re.subn(r"\.db$", "", my.dbfile)
    if was_db:
        pass
    else:
        yaml_filename, was_sqlite = re.subn(r"\.sqlite$", "", my.dbfile)
    yaml_filename = yaml_filename + ".yaml"
    try:
        yamfl = open(yaml_filename, 'r')
        spec_dict = yaml.safe_load(yamfl)
        yamfl.close()
    except IOError as e:
        spec_dict = {'data': {}, 'dbfile': my.dbfile}
    setup_win = Tkinter.Tk()
    setup_win.title("setup " + table_name)
    Tkinter.Label(setup_win, text=table_name).pack()
    inner_frame = Tkinter.Frame(setup_win)
    inner_frame.pack()
    clms = list(ex("pragma table_info(" + table_name +")"))
    Tkinter.Label(inner_frame, text="column").grid(column = 0, row = 0)
    Tkinter.Label(inner_frame, text="human readable name").grid(column = 1, row = 0)
    Tkinter.Label(inner_frame, text="number of rows").grid(column = 2, row = 0)
    Tkinter.Label(inner_frame, text="show on collect(0/1)?").grid(column = 3, row = 0)
    lbls = {} ; hr_name_flds = {} ; nrows_flds = {} ; show_flds = {} ;
    for c in clms:
        lbls[c] = Tkinter.Label(inner_frame, text=c[1])
        lbls[c].grid(column = 0, row = len(lbls.keys()))
        hr_name_flds[c] = Tkinter.Entry(inner_frame, width = 15)
        hr_name_flds[c].grid(column = 1, row = len(lbls.keys()))
        if table_name in spec_dict['data'].keys():
            hr_name_flds[c].insert(
                Tkinter.END,
                spec_dict['data'][table_name]['columns'][len(lbls.keys()) - 1][1]
                )
        else:
            hr_name_flds[c].insert(Tkinter.END, c[1])
        nrows_flds[c] = Tkinter.Entry(inner_frame, width = 2)
        nrows_flds[c].grid(column = 2, row = len(lbls.keys()))
        if table_name in spec_dict['data'].keys():
            nrows_flds[c].insert(
                Tkinter.END,
                spec_dict['data'][table_name]['columns'][len(lbls.keys()) - 1][2]
                )
        else:
            nrows_flds[c].insert(Tkinter.END,"2")
        show_flds[c] = Tkinter.Entry(inner_frame, width = 1)
        show_flds[c].grid(column = 3, row = len(lbls.keys()))
        if table_name in spec_dict['data'].keys():
            show_flds[c].insert(
                Tkinter.END,
                spec_dict['data'][table_name]['columns'][len(lbls.keys()) - 1][3]
                )
        else:
            show_flds[c].insert(Tkinter.END,"1")
    def setup_fn(event = None):
        new_table = {table_name: {'columns': [[c[1],
                                               hr_name_flds[c].get(),
                                               int(nrows_flds[c].get()),
                                               int(show_flds[c].get())] for c in clms],
                                  'tablename': table_name}}
        spec_dict['data'].update(new_table)
        yamfl_w = open(yaml_filename, 'w')
        yamfl_w.write(yaml.safe_dump(spec_dict))
        yamfl_w.close()
        setup_win.destroy()
    Tkinter.Button(inner_frame,
                   text = "SET",
                   command = setup_fn).grid(column = 0, row=len(lbls.keys()) + 1)
    setup_win.wait_window()

def choose_table_for_starter(charhint):
    chooser_win = Tkinter.Tk()
    chooser_win.title("choose a table:")
    inner_frame = Tkinter.Frame(chooser_win)
    inner_frame.pack()
    def starter_fn(ch):
        def inner_fn(x):
            chooser_win.destroy()
            eval("""starter(data."""+charhint[ch]+")")
        return inner_fn
    b = {}
    for ch in charhint.keys():
        b[ch] = Tkinter.Button(inner_frame,
                               text = ch + ":" + charhint[ch],
                               command = starter_fn(ch)
                               )
        b[ch].pack()
        chooser_win.bind("<"+ch+">", starter_fn(ch))
    chooser_win.mainloop()


if __name__=='__main__':
#    from IPython.Shell import IPShellEmbed
    import IPython
    from optparse import OptionParser
    # from linii import *

    parser = OptionParser()

    parser.add_option("-y", "--yaml", dest="yaml_file", metavar="YAML_FILE", help="""specify .yaml file""")
    parser.add_option("--dbfile", dest="db_file", metavar="SQLITE_FILE", help="""specify .db or .sqlite file""")
    parser.add_option("--setup", dest="do_setup", action="store_true", default=False, help="""build the .yaml file from the .db file""")
    parser.add_option("-i", "--interactive", dest="do_start_ipython", action="store_true", default=False,
                      help="""start ipython""")


    (options, args) = parser.parse_args()

    if options.yaml_file:
        my.yamlfile = options.yaml_file
        read_yaml(my.yamlfile)

    if data:
        ip_banner = "available tables:  " + ", ".join([ 'data.'
                                                        + x for x in filter(lambda u: u[:2]!='__',
                                                                            dir(data))])
    else:
        ip_banner = ""


    if options.do_setup:
        if not options.db_file:
            raise my.LiniiError("FORGOT TO SPECIFY SQLITE FILE")
        else:
            my.dbfile = options.db_file
            for tbl in list_of_tables():
                setup_yaml(tbl)
    elif options.do_start_ipython:
        print(ip_banner)
        ipshell = IPython.embed(
            # argv = ["-colors", "LightBG"],
            # banner = ip_banner,
            # exit_msg = 'Leaving Interpreter, back to program.'
            )
    else:
        charhint = dict(zip(my.alphabet, filter(lambda u: u[:2] != '__', dir(data))))
        choose_table_for_starter(charhint)
        # print "Choose table: "
        # print "  ".join([ c + ":" + charhint[c] for c in charhint.keys()])
        # inpt = my.Getch(my)()
