#!/usr/bin/python

__author__    = "Andrei Mikhailov"
__copyright__ = "Copyright 2013, Andrei Mikhailov"
__license__   = "GPL"


import os
from datetime import date, timedelta
from itertools import product 
from optparse import OptionParser
from tables.addr import *

parser = OptionParser()
parser.add_option("-a", "--all", dest="do_show_all", action="store_true", default=False,
                  help="""show all""", metavar="NEW_ENTRY")
(options, args) = parser.parse_args()

if not(options.do_show_all):
    import pygtk
    pygtk.require('2.0')
    import gtk

querystr = "message,months,days,advance,dismissed"

def start_over(datalines):
    print("about to start over")
    prepare_and_show_gui(datalines)

def destroy(widget, data=None):
    gtk.main_quit()
    gtk.gdk.flush()

def dismiss(message):
    linii.ex("UPDATE monthly SET dismissed = '" + date.today().isoformat() + "' WHERE message = '" + message + "'")
    start_over(read_data())

def define_new():
    linii.collect(monthly)
    start_over(read_data())

def modify(message):
    linii.update(monthly,"WHERE message = '"+message+"'")
    start_over(read_data())

def show_all():
    linii.buttons(monthly,"",querystr)
    print("exited from buttons in monthly")
    # linii.starter(monthly, default_checked=querystr)
    start_over(read_data())

def prepare_and_show_gui(datalines):

    gtk.rc_parse(os.environ['HOME'] +"/.monthlyrc")
    window = gtk.Window(type=gtk.WINDOW_TOPLEVEL)
    window.set_wmclass("todo-reminder","todo-reminder")
    window.set_type_hint(gtk.gdk.WINDOW_TYPE_HINT_DIALOG)
    window.move(750,0)
    window.connect("destroy",destroy)
    
    vbox = gtk.VBox()
    window.add(vbox)
    vbox.show()
    
    for line in datalines: 
      if ('error' in line.keys()):
        button_text = line['message']+' ! '+line['error']
      else :
        button_text = line['message'] +'='*(((date.today()) - line['first_match']).days)
      button = gtk.Button(button_text)
      def clicked_itm(vbx,win,ln) :
        def retfn(widget_which_was_clicked):
            vbx.destroy()
            prepare_and_show_dialogue(win, ln)
        return retfn
      button.connect("clicked", clicked_itm(vbox, window, line['message']))
      vbox.add(button)
      button.show()
    
    hbox_bottom = gtk.HBox()
    vbox.add(hbox_bottom)
    hbox_bottom.show()

    def refresh_fn(window) :
        def retfn(widget_which_was_clicked) :
            window.destroy()
            gtk.gdk.flush()
            start_over(read_data())
        return retfn
    refresh_btn = gtk.Button("reload")
    refresh_btn.connect("clicked", refresh_fn(window));
    hbox_bottom.add(refresh_btn)
    refresh_btn.show()
    def new_fn(window) :
        def retfn(widget_which_was_clicked) :
            window.destroy()
            gtk.gdk.flush()
            define_new()
        return retfn
    new_btn = gtk.Button("new")
    new_btn.connect("clicked", new_fn(window));
    hbox_bottom.add(new_btn)
    new_btn.show()
    def all_fn(win) :
        def retfn(widget_which_was_clicked):
            win.destroy()
            gtk.gdk.flush()
            show_all()
        return retfn
    all_btn = gtk.Button("show all")
    all_btn.connect("clicked", all_fn(window));
    hbox_bottom.add(all_btn); all_btn.show()
    def exit_fn(window) :
        def retfn(widget_which_was_clicked) :
            window.destroy()
            gtk.gdk.flush()
        return retfn 
    exit_btn = gtk.Button("exit")
    exit_btn.connect("clicked", exit_fn(window));
    hbox_bottom.add(exit_btn)
    exit_btn.show()

    window.show()
    gtk.main()

    
def prepare_and_show_dialogue(window,message) :
    window.resize(80,8)
    window.move(100,10)
    vbnew = gtk.VBox()
    window.add(vbnew)
    ttlab = gtk.Label(message);
    vbnew.add(ttlab);
    ttlab.show(); vbnew.show();
    hb = gtk.HBox(); vbnew.add(hb); hb.show()
    dismiss_btn = gtk.Button("forget it")
    def dismiss_fn(window,message) :
        def retfn(widget_which_was_clicked) :
            window.destroy()
            gtk.gdk.flush()
            dismiss(message)
        return retfn
    dismiss_btn.connect("clicked", dismiss_fn(window,message));
    hb.add(dismiss_btn); dismiss_btn.show()
    def modify_fn(win,msg) :
        def retfn(widget_which_was_clicked):
            win.destroy()
            gtk.gdk.flush()
            modify(msg)
        return retfn
    modify_btn = gtk.Button("modify")
    modify_btn.connect("clicked", modify_fn(window,message));
    hb.add(modify_btn); modify_btn.show()
    def do_nothing_fn(window) :
        def retfn(widget_which_was_clicked) :
            window.destroy()
            gtk.gdk.flush()
            start_over(read_data())
        return retfn
    do_nothing_btn = gtk.Button("do nothing about it")
    do_nothing_btn.connect("clicked", do_nothing_fn(window));
    vbnew.add(do_nothing_btn); do_nothing_btn.show()


def daterange(start_date, end_date):
    for n in range((end_date - start_date).days + 1):
        yield start_date + timedelta(n)


def correct_if_previous_month(month, day_before_end_of_month):
    yeartoday = date.today().year
    if (day_before_end_of_month > 0) : 
        corrected = (month, day_before_end_of_month)
    else : 
        first_of_the_month = date(yeartoday, month, 1)
        date_in_prev_month = first_of_the_month - ( - day_before_end_of_month + 1 ) 
        corrected = (date_in_prev_month.month , date_in_prev_month.day)
    return corrected


def subtract_advance(month,day,advance):
    yeartoday = date.today().year
    day_to_start_warning = date(yeartoday,month,day)
    if advance:
        day_to_start_warning = day_to_start_warning - timedelta(advance)
    month_and_day_returned = (day_to_start_warning.month, day_to_start_warning.day)
    return month_and_day_returned


def is_leap_year(y):
    return ((((y % 4) == 0) and ((y % 100) != 0)) or ((y % 400) == 0)) 


def augment(row):
    """adds additional fields to the associative array"""
    proceed_to_check_matches = True
    today = date.today()
    if (row['months'] == 'all') :
        ms = [1,2,3,4,5,6,7,8,9,10,11,12]
    elif (row['months'] == 'odd'):
        ms = [1,3,5,7,9,11]
    elif (row['months'] == 'even'):
        ms = [2,4,6,8,10,12]
    else :
        try :
            ms = map(int, row['months'].split(","))
            if ( len(ms) == 0 ) :
                row['error'] = 'MISSING MONTHS'
                proceed_to_check_matches = False
            elif not( reduce( lambda x,y : x and y ,
                                map( lambda z : (z>0) and (z<13) , ms ) ,
                                True ) ) : 
                row['error'] = 'MONTH OUT OF RANGE'
                proceed_to_check_matches = False
        except ValueError :
            row['error'] = 'ERROR PARSING MONTHS'
            proceed_to_check_matches = False

    if proceed_to_check_matches :
        try : 
            ds = map(int, row['days'].split(","))
            if ( len(ds) == 0 ) :
                row['error'] = 'MISSING DAYS'
                proceed_to_check_matches = False
            elif not( reduce( lambda x,y : x and y ,
                                map( lambda z : ( z < 29 ) , ds ) ,
                                True ) ) : 
                row['error'] = 'DAY OUT OF RANGE (cannot be > 28)'
                proceed_to_check_matches = False
        except ValueError :
            row['error'] = 'ERROR PARSING DAYS'
            proceed_to_check_matches = False

    if proceed_to_check_matches :
        mds = list(product(ms,ds))
        mds = map( lambda x: correct_if_previous_month(*x) , mds)
        adv = 0
        if row['advance']:
            try:
                adv = int(row['advance'])
            except ValueError :
                row['error'] = row['error'] + " and " +'ERROR PARSING ADVANCE'
        mds = map( lambda x: subtract_advance(x[0], x[1], adv) ,  mds)
        if (row['dismissed'] != "") :
          dismissed = date(*map(int, row['dismissed'].split("-")))
          if ( (dismissed + timedelta(366)) > today ) :
            list_of_passed_days = list(daterange(dismissed + timedelta(1), today))
            for x in list_of_passed_days:
              if ( (x.month, x.day) in mds ): 
                row['first_match'] = x
                break
          else :
            row['error'] = 'VERY OLD'
        else :
          row['error'] = 'NEVER DISMISSED'

    if ('error' in row.keys()): 
        print row['error']

def read_data() :
    # First read from database:
    names = querystr.split(",")
    rows = list(linii.ex("SELECT " + querystr + " FROM monthly"))
    rowdict = map( lambda x: dict(zip(names,x)) , rows )
    map(augment, rowdict)
    return filter(lambda a: ('first_match' in a.keys()) or ('error' in a.keys()) , rowdict)



if __name__ == '__main__': 


    if options.do_show_all:
        linii.buttons(monthly,"",querystr)
    else:

        prepare_and_show_gui(read_data())
