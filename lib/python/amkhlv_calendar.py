#!/usr/bin/env python
# -*- coding: utf-8 -*-

__author__ = "Andrei Mikhailov"
__copyright__ = "Copyright 2013, Andrei Mikhailov"
__license__ = "GPL"

"""My calendar. Actually not so bad, considering the alternatives."""

from optparse import OptionParser
import sys
from os import getenv, remove, system
from datetime import date, datetime, timedelta
import cgi

import vobject
from dateutil.parser import *
from dateutil.rrule import *
from pytz import timezone


try:
    import Tkinter
except ImportError:
    pass
import pickle
import re
import subprocess
import urwid
import yaml


DEFAULT_FILENAME = getenv("HOME") + "/a/calendars/calendar.ics"
DEFAULT_JOURNAL_FILENAME = getenv("HOME") + "/.local/share/evolution/memos/system/journal.ics"
DEFAULT_TODOLIST_FILENAME = getenv("HOME") + "/.local/share/evolution/tasks/system/tasks.ics"
SERIAL_FILE = getenv("HOME") + "/.clndr-serial"

HUMAN_FLDS = {'VEVENT': ('DTSTART', 'DTEND', 'SUMMARY', 'LOCATION', 'DESCRIPTION', 'RRULE'),
              'VTODO': ('SUMMARY', 'DUE', 'PRIORITY'),
              'VJOURNAL': ('SUMMARY', 'DESCRIPTION')}

DATE_FLDS = ['DTSTART', 'DTEND', 'DUE']

# TEXT_FLDS are those for which we use Text widget rather than Entry widget
TEXT_FLDS = ['DESCRIPTION']

CLR_STR = {'NORM': "[0m",
           'BG_RED': "[41m", 'BG_YELL': "[43m", 'BG_BLUE': "[44m",
           'RED': "[31m", 'BRT_RED': "[1;31m",
           'YELL': "[33m", 'BRT_YELL': "[1;33m",
           'WHT': "[37m", 'BRT_WHT': "[1;37m"}

WM_WINDOW_CLASS = "zenity"
WM_WINDOW_GEOMETRY = "+300+300"

def get_timezone():
    with open("/etc/timezone") as f:
            etc_timezone = f.readlines()
            # tz_here = timezone(etc_timezone[0].strip())
            tz = timezone(etc_timezone[0].strip())
    return tz


tz_here = get_timezone()

# _Getch is from http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/134892
class _Getch:
    """Gets a single character from standard input.  Doesn't echo to screen."""

    def __init__(self):
        try:
            self.impl = _GetchWindows()
        except ImportError:
            self.impl = _GetchUnix()

    def __call__(self):
        return self.impl()


class _GetchUnix:
    def __init__(self): pass

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
    def __init__(self): pass

    def __call__(self):
        import msvcrt

        return msvcrt.getch()


getch = _Getch()


def prnt(s):
    print(s.encode('utf-8'))


def loose_tz(dt):
#    return dt.replace(tzinfo=pytz.UTC)
    if __name__ != '__main__' or options.tz =="" : tz_here = get_timezone()
    else: tz_here = timezone(options.tz)
    return dt.replace(tzinfo=tz_here)


def human2machine(x, val_str, params={}):
    """returns ContentLine"""
    if val_str == "":
        return None
    elif x in DATE_FLDS:
        v = parse(val_str)
        if params == {}:
            ps = [["TZID", tz_here.zone]]
        else:
            ps = [[p_name] + params[p_name]
                  for p_name in params.keys()]
            # I dont know why I need this; I did not manage to figure out what is X-VOBJ-ORIGINAL-TZID
            # and why TZID gets replaced wit X-VOBJ-ORIGINAL-TZID, but I do not want it, 
            # I want TZID otherwize the tzinfo does not come out right!
            def pname_replacer(x):
                if x[0][:16] == "X-VOBJ-ORIGINAL-": x[0] = x[0][16:]

            map(pname_replacer, ps)
        return vobject.base.ContentLine(x, ps, vobject.icalendar.dateTimeToString(v))
    else:
        return vobject.base.ContentLine(x,
                                        [p_name + params[p_name]
                                         for p_name in params.keys()],
                                        val_str)


def machine2human(x, y):
    """returns string"""
    if x in DATE_FLDS:
        try:
            return y.strftime("%Y-%m-%d at %H:%M")
        except AttributeError:
            return y
    else:
        return y


def write_calendar(cal, fname):
    with open(fname, "w") as fl:
        vobject.base.defaultSerialize(cal, fl, 100)
        fl.close()


def read_calendar(fname):
    with open(fname, 'r') as fl:
        cl = list(vobject.readComponents(fl))
    return cl[0]


def get_events(fname):
    return read_calendar(fname).components()


def build_calendar(events):
    new_cal = vobject.base.Component(name='VCALENDAR')
    for ev in events:
        new_cal.add(ev)
    return new_cal


def mywalk(evs, tdy=None, frw=0, rwnd_hrs=0, rgx=None):
    """ Returns the list of pairs (event_dictionary, occ)"""
    if not rgx:
        dt_min = tdy - timedelta(hours=rwnd_hrs)
        dt_max = tdy + timedelta(hours=24 * (1 + frw))
    inrange = []
    for ev in evs:
        evcomp = list(ev.components())
        # TODO: add support for multicomponents, e.g. ALARMS
        lns = ev.lines()
        ev_dict = dict((ln.name, ln.value) for ln in lns)
        if rgx:
            if ( ('SUMMARY' in ev_dict.keys()) and
                     re.match(rgx, ev_dict['SUMMARY']) ):
                inrange.append((ev_dict, None))
        elif ('RRULE' in ev_dict.keys()):
            try:
                for occ in ev.getrruleset().between(dt_min, dt_max, True):
                    inrange.append((ev_dict, occ))
            except TypeError:
                dt_min_tznv = dt_min.replace(tzinfo=None)
                dt_max_tznv = dt_max.replace(tzinfo=None)
                for occ in ev.getrruleset().between(dt_min_tznv, dt_max_tznv, True):
                    inrange.append((ev_dict, occ))
        elif ('DTSTART' in ev_dict.keys()):
            dtst = ev_dict['DTSTART']
            if dtst.__class__ == date.today().__class__:
                # print "converting date to datetime"
                # print dtst
                dtst = (datetime.now() -
                        timedelta(hours = options.offset if __name__ == '__main__' else 0 )
                ).replace(day=dtst.day, month=dtst.month, year=dtst.year)
            dtst = loose_tz(dtst)
            if (dtst > dt_min) and (dtst < dt_max):
                inrange.append((ev_dict, None))
        else:
            pass #TODO: the entries without DTSTART what are they?
    return (inrange)


def Tk_update_dict(a, tkwin, set_dtend=False):
    """a is dict of type: {"key": ["value",n]} where n is intended order of line in form"""
    grd = Tkinter.Frame(tkwin)
    e = {};
    v = {}
    for k in sorted(a.keys(),
                    key=lambda x: a[x][1]):
        Tkinter.Label(grd, text=k, font="Terminus 18").grid(row=a[k][1], column=0, sticky=Tkinter.E)
        if k in TEXT_FLDS: # use Text widget
            e[k] = Tkinter.Text(grd)
            e[k].grid(row=a[k][1], column=1)
            e[k].height = 7
            e[k].insert(1.0, a[k][0])
        else: # use Entry widget
            v[k] = Tkinter.StringVar()
            v[k].set(a[k][0])
            e[k] = Tkinter.Entry(grd, textvariable=v[k])
            e[k].grid(row=a[k][1], column=1)
        grd.pack()
        bt_frm = Tkinter.Frame(tkwin)

    def collect():
        a0 = {}
        for k in a.keys():
            if k in TEXT_FLDS: # use Text widget
                a0[k] = e[k].get(1.0, Tkinter.END)
            else: #: use Entry widget
                a0[k] = v[k].get()
        for k in a.keys():
            if k != 'DTEND' or ( a0[k] != '' and a0[k][0] != '+'):
                a[k][0] = a0[k]
            else:  # set DTEND automatically 
                m_days = re.match(r".*?(\d+)d.*", a0[k])
                duration_days = int(m_days.group(1)) if m_days else 0
                m_hours = re.match(r".*?(\d+)h.*", a0[k])
                duration_hours = int(m_hours.group(1)) if m_hours else 0
                m_mins = re.match(r".*?(\d+)m.*", a0[k])
                duration_mins = int(m_mins.group(1)) if m_mins else 0
                # print "mins:"+str(duration_mins)+"hours:"+str(duration_hours)+"days:"+str(duration_days)
                if m_days or m_hours or m_mins:
                    adt = ( parse(a0['DTSTART']) +
                            timedelta(minutes=duration_mins +
                                              60 * duration_hours + 24 * 60 * duration_days)
                    )
                else:
                    adt = parse(a0['DTSTART']) + timedelta(hours=1)
                a[k][0] = adt.strftime("%Y-%m-%d at %H:%M")
        tkwin.destroy()

    btn = Tkinter.Button(bt_frm, text="Enter", command=collect)
    btn.pack()
    bt_frm.pack()


def urwid_update_dict(a, set_dtend=False):
    """a is dict of type: {"key": ["value",n]} where n is intended order of line in form"""
    urwid.set_encoding('utf-8')
    e = {};
    sorted_keys = sorted(a.keys(), key=lambda x: a[x][1])
    key = dict(zip(range(1, 100), sorted_keys))
    urwid.set_encoding("utf-8")

    def show_all_input(inp, raw):
        # this one we do not use
        return [unicode(i.encode('utf-8')) for i in inp]

    def trivial(inp, raw):
        return inp

    for k in sorted_keys:
        e[k] = urwid.Edit(caption=k + u" ", edit_text= u"" + unicode(a[k][0]), multiline=True)

    def collect(button):
        print("----HERE---")
        a0 = {}
        for k in a.keys():
            a0[k] = e[k].get_edit_text()
        for k in a.keys():
            if k != 'DTEND' or ( a0[k] != '' and a0[k][0] != '+'):
                a[k][0] = a0[k].encode('latin-1')
            else:  # set DTEND automatically 
                m_days = re.match(r".*?(\d+)d.*", a0[k])
                duration_days = int(m_days.group(1)) if m_days else 0
                m_hours = re.match(r".*?(\d+)h.*", a0[k])
                duration_hours = int(m_hours.group(1)) if m_hours else 0
                m_mins = re.match(r".*?(\d+)m.*", a0[k])
                duration_mins = int(m_mins.group(1)) if m_mins else 0
                # print "mins:"+str(duration_mins)+"hours:"+str(duration_hours)+"days:"+str(duration_days)
                if m_days or m_hours or m_mins:
                    adt = ( parse(a0['DTSTART']) +
                            timedelta(minutes=duration_mins +
                                              60 * duration_hours + 24 * 60 * duration_days)
                    )
                else:
                    adt = parse(a0['DTSTART']) + timedelta(hours=1)
                a[k][0] = adt.strftime("%Y-%m-%d at %H:%M")
        raise urwid.ExitMainLoop()
        print("----THERE")

    btn = urwid.Button("OK")
    urwid.connect_signal(btn, 'click', collect)
    walker = urwid.SimpleFocusListWalker([e[k] for k in sorted_keys] + [btn])
    listBox = urwid.ListBox(walker)

    def shift_focus_on_tab(key):
        maxpos = len(walker.positions())
        focus_posn = walker.focus
        if key in ['tab']:
            if ((focus_posn + 1) < maxpos):
                walker.set_focus(focus_posn + 1)
                if ((focus_posn + 2) < maxpos):
                    walker[focus_posn + 1].insert_text(u"")
                    btn.set_label("OK")
                else: # we are on the button
                    btn.set_label("OK *")
            else:
                walker.set_focus(0)
                walker[0].insert_text(u"")
                btn.set_label("OK")
        if key in ['shift tab']:
            if (focus_posn > 0):
                walker.set_focus(focus_posn - 1)
                walker[focus_posn - 1].insert_text(u"")
                btn.set_label("OK")
            else:
                walker.set_focus(maxpos - 1)
                btn.set_label("OK *")

    loop = urwid.MainLoop(listBox,
                          input_filter=trivial,
                          unhandled_input=shift_focus_on_tab)
    loop.run()


def delete_event(uid, evs):
    deleted_evs = []

    def uid_filter_out(ev):
        lns = ev.lines()
        ev_dict = dict((ln.name, ln.value) for ln in lns)
        if 'UID' in ev_dict.keys():
            if ev_dict['UID'] != uid:
                return True
            else:
                deleted_evs.append(ev)
                return False
            return (ev_dict['UID'] != uid)
        else:
            return True

    remaining_evs = filter(uid_filter_out, evs)
    if len(deleted_evs) > 1:
        print("*** ERROR: MORE THAN ONE UID MATCH ***")
    return deleted_evs, remaining_evs


def collect_ev(ev_type):
    new_ev = vobject.icalendar.RecurringComponent(name=ev_type)
    event_dict = dict([( HUMAN_FLDS[ev_type][j], ["", j] )
                       for j in range(len(HUMAN_FLDS[ev_type]))])
    urwid_update_dict(event_dict)
    # Tk_update_dict(event_dict, root); root.mainloop()
    for k in event_dict.keys():
        new_contline = human2machine(k, event_dict[k][0], {})
        if new_contline != None:
            new_ev.add(new_contline)
    return new_ev


def add_vevent(fname):
    new_ev = collect_ev('VEVENT')
    lns = new_ev.lines()
    lnames = [ln.name for ln in lns]
    if 'DTSTART' in lnames:
        if 'SUMMARY' in lnames:
            cl = read_calendar(fname)
            cl.add(new_ev)
            write_calendar(cl, fname)
            print("ADDED VEVENT:")
            new_ev.prettyPrint()
        else:
            raise ValueError("*** Missing event summary ***")
    else:
        raise ValueError("*** Missing event start date ***")


def add_vjournal(fname):
    new_vj = collect_ev('VJOURNAL')
    cl = read_calendar(fname)
    cl.add(new_vj)
    write_calendar(cl, fname)
    print("ADDED VJOURNAL:")
    new_vj.prettyPrint()


def add_vtodo(fname):
    new_vtd = collect_ev('VTODO')
    cl = read_calendar(fname)
    cl.add(new_vtd)
    write_calendar(cl, fname)
    print("ADDED VTODO:")
    new_vtd.prettyPrint()


def update_events(evs, cl, fname, ev_type):
    for ev in evs:
        lns = ev.lines()
        ecomps = ev.components()
        old_ev_dict = dict((ln.name, {'VL': ln.value, 'PRMS': ln.params}) for ln in lns)
        missing_hfs = filter(lambda x: x not in old_ev_dict.keys(), HUMAN_FLDS[ev_type])
        old_ev_dict.update([(f, {'VL': "", 'PRMS': {}}) for f in missing_hfs])
        old_ev_dict_human = dict([( f, {'VL': machine2human(f, old_ev_dict[f]['VL']),
                                        'PRMS': old_ev_dict[f]['PRMS']} )
                                  for f in old_ev_dict.keys()])
        new_ev = vobject.icalendar.RecurringComponent(name=ev_type)
        new_ev_dict = dict([( HUMAN_FLDS[ev_type][j],
                              [old_ev_dict_human[HUMAN_FLDS[ev_type][j]]['VL'], j] )
                            for j in range(len(HUMAN_FLDS[ev_type]))])
        # Tk_update_dict(new_ev_dict, root); root.mainloop()
        urwid_update_dict(new_ev_dict)
        for k in new_ev_dict.keys():
            new_contline = human2machine(k, new_ev_dict[k][0], old_ev_dict[k]['PRMS'])
            if new_contline != None:
                new_ev.add(new_contline)
        for c in ecomps: new_ev.add(c)
        new_ev.prettyPrint()
        #TODO: ecomps contain ALARMs and other such things; we may want to provide a way 
        #to modify them, too
        cl.add(new_ev)
        write_calendar(cl, fname)
        print("UPDATED EVENT:")
        new_ev.prettyPrint()


def location_str(x):
    # I decided that I dont want to show location:
    return ""
    # if 'LOCATION' in x.keys():
    #     return " LOCATION:" + x['LOCATION'] 
    # else: return ""


def aux_time_from_pair(x):
    if x[1]:
        return loose_tz(x[1])
    else:
        return loose_tz(x[0]['DTSTART'])


def show_items(inrange, serObj, do_order=True, blank_days=[]):
    inr = [(aux_time_from_pair(u), u) for u in inrange]
    inr = inr + filter(lambda u: not ( u[0].strftime("%a %d %b") in [v[0].strftime("%a %d %b") for v in inr] ),
                       blank_days)
    inr_sorted = sorted(inr, key=lambda u: u[0]) if do_order else inr
    adbPrev = ""
    for x in inr_sorted:
        prefix = "";
        week_day = "";
        week_day_n = "0";
        if x[1]:
            serObj.append(x[1][0]['UID'])
            prefix = "[" + str(serObj.length()) + "] "
            if serObj.length() < 10 and prefix: prefix = " " + prefix
            if x[1][1] == None:
                adb = x[1][0]['DTSTART'].strftime("%a %d %b")
                if adb != adbPrev:
                    prnt(prefix + x[1][0]['DTSTART'].strftime("%a %d %b at %H:%M : ") +
                         x[1][0]['SUMMARY'] + location_str(x[1][0]))
                else:
                    prnt(prefix + x[1][0]['DTSTART'].strftime("           at %H:%M : ") +
                         x[1][0]['SUMMARY'] + location_str(x[1][0]))
                adbPrev = adb
                week_day = x[1][0]['DTSTART'].strftime("%a");
                week_day_n = x[1][0]['DTSTART'].strftime("%w")
            else: #for repeating events:
                adb = x[1][1].strftime("%a %d %b")
                if adb != adbPrev:
                    prnt(prefix + x[1][1].strftime("%a %d %b at %H:%M : ") +
                         x[1][0]['SUMMARY'] + " [repeating] " + location_str(x[1][0]))
                else:
                    prnt(prefix + x[1][1].strftime("           at %H:%M : ") +
                         x[1][0]['SUMMARY'] + " [repeating] " + location_str(x[1][0]))
                adbPrev = adb
                week_day = x[1][1].strftime("%a");
                week_day_n = x[1][1].strftime("%w")
        else:  #blank day
            prefix = 5 * " "
            prnt(prefix + x[0].strftime("%a %d %b"))
            week_day = x[0].strftime("%a");
            week_day_n = x[0].strftime("%w")
        if int(week_day_n) == 6:
            print(u"☀" * 24).encode('utf-8')


def list_items(inrange, serObj):
    for x in inrange:
        prefix = "";
        serObj.append(x[0]['UID'])
        prefix = "[" + str(serObj.length()) + "] "
        if x[1] == None:
            prnt(prefix + x[0]['DTSTART'].strftime("%a %d %b at %H:%M : ") +
                 x[0]['SUMMARY'] + location_str(x[0]))
        else: #for repeating events:
            prnt(prefix + x[1].strftime("%a %d %b at %H:%M : ") +
                 x[0]['SUMMARY'] + " [repeating] " + location_str(x[1][0]))


def show_journal(evs, serObj):
    for ev in evs:
        evcomp = list(ev.components())
        # TODO: add support for multicomponents, e.g. ALARMS
        lns = ev.lines()
        ev_dict = dict((ln.name, ln.value) for ln in lns)
        if ('DESCRIPTION' in ev_dict.keys()) or ('SUMMARY' in ev_dict.keys()):
            if ('UID' in ev_dict.keys()):
                serObj.append(ev_dict['UID'])
                prefix = "[" + str(serObj.length()) + "] "
            else:
                prefix = ""
            prnt(prefix + ev_dict['SUMMARY'])


def show_todolist(evs, serObj):
    for ev in evs:
        # TODO: add support for multicomponents, e.g. ALARMS
        lns = ev.lines()
        ev_dict = dict((ln.name, ln.value) for ln in lns)
        clr_string = ""
        if ('SUMMARY' in ev_dict.keys()):
            if ('UID' in ev_dict.keys()):
                serObj.append(ev_dict['UID'])
                prefix = "[" + str(serObj.length()) + "] "
            else:
                prefix = ""
            if ('PRIORITY' in ev_dict.keys()):
                if int(ev_dict['PRIORITY']) < 6:
                    clr_string = CLR_STR['BG_BLUE'] + CLR_STR['BRT_YELL']
                if int(ev_dict['PRIORITY']) < 3:
                    clr_string = CLR_STR['BG_YELL'] + CLR_STR['BRT_RED']
            prnt(prefix + clr_string + ev_dict['SUMMARY'] + CLR_STR['NORM'])


def convert_to_gtd(evs):
    import linii
    linii.read_yaml("/home/andrei/a/tech/base/addr.yaml")
    print(linii.my.dbfile)
    new_evs = evs
    for ev in evs:
        lns = ev.lines()
        ev_dict = dict((ln.name, ln.value) for ln in lns)
        if ('SUMMARY' in ev_dict.keys()):
            linii.collect(linii.data.gtd, None, task=ev_dict['SUMMARY'])
            if ('UID' in ev_dict.keys()):
                _, new_evs = delete_event(ev_dict['UID'], new_evs)
    write_calendar(build_calendar(new_evs), fname)


def specify_dates(tdy,threeweeks):
    dt_cur_month = tdy.month
    dt_cur_week_starts = (tdy -
                          timedelta(days=((tdy.weekday() + 1) % 7))
                      ).replace(hour=0, minute=0, second=1)
    dt_cur_month_starts = (tdy -
                           timedelta(days=(tdy.day - 1))
                       ).replace(hour=0, minute=0, second=1)
    if threeweeks:
        if tdy.weekday() > 1 and tdy.weekday() != 6:
            dt_display_starts = dt_cur_week_starts
        else:
            dt_display_starts = dt_cur_week_starts - timedelta(days=7)
    else:
        dt_display_starts = (dt_cur_month_starts -
                             timedelta(days=(dt_cur_month_starts.weekday() + 1) % 7))
    return dt_cur_month, dt_cur_week_starts, dt_cur_month_starts, dt_display_starts


def my_serialize(x, tdy):
    if x.__class__ == tdy.__class__:
        return x.isoformat()
    else:
        return str(x)


def gen_xml(evs, tdy):
    from lxml import etree

    dt_cur_month, dt_cur_week_starts, dt_cur_month_starts, dt_display_starts = specify_dates(tdy, threeweeks=options.threeweeks)
    inrange = mywalk(evs, tdy=tdy, frw=options.forw, rwnd_hrs=8, rgx=options.rgx)
    root = etree.Element("events")
    for x in inrange:
        ev = etree.Element("event")
        for k in x[0].keys():
            ev.set(k, my_serialize(x[0][k], tdy))
        root.append(ev)
    print(etree.tostring(root, pretty_print=True))


def gen_html(evs, tdy, serObj=None, threeweeks=False, elinks=False):
    dt_cur_month, dt_cur_week_starts, dt_cur_month_starts, dt_display_starts = specify_dates(tdy, threeweeks)
    result = """<html>
<head>
<title>11/11</title>
<link rel="stylesheet" href="calstyle.css">
<meta http-equiv="refresh" content="1800"> 
</head>
<body>
<TD valign="top" width="100%"><a name=_1111>
<table class="calendar" width=100% border=1>
<tr><th class="calendar"width=14%>Sunday</th><th class="calendar"width=14%>Monday</th><th class="calendar"width=14%>Tuesday</th><th class="calendar"width=14%>Wednesday</th><th class="calendar"width=14%>Thursday</th><th class="calendar"width=14%>Friday</th><th class="calendar"width=14%>Saturday</th></tr>""" + "\n"

    def mycgi(ev):
        if serObj:
            serObj.append(ev[0]['UID'])
            post_fix = "(" + str(serObj.length()) + ")"
        else:
            post_fix = ""
        return ( cgi.escape(ev[0]['SUMMARY']) +
                 """<span style="display:inline;color:#ff5f00">""" + post_fix + """</span>""" )

    weekrange = range(3) if threeweeks else range(5)
    for w in weekrange:
        result = result + "<tr>\n"
        for d in range(7):
            running_dt = dt_display_starts + timedelta(days=d + 7 * w)
            running_day = running_dt.day
            running_month = running_dt.month
            tdy_events = mywalk(evs, tdy=running_dt)
            n_evs = len(tdy_events)
            mysep = "<br>&nbsp;"
            if tdy_events != []:
                first_event = mycgi(tdy_events[0])
            else:
                first_event = ''
            data_str = mysep.join([mycgi(x) for x in tdy_events[1:]]) + (mysep * (2 - n_evs))
            class_str = """ class="thismonth" """
            clr_str = ""
            if running_month < dt_cur_month:
                class_str = """ class="prevmonth" """
            elif running_month > dt_cur_month:
                class_str = """ class="nextmonth" """
            elif tdy.day == running_day:
                class_str = """ class="today" """
                clr_str = """ style="background-color:#ff0000" """ if elinks else ""
            result = result + "<td valign=top" + class_str + ">"
            result = ( result + '<table><td class="datestr"><div ' + clr_str + '><b>' +
                       str(running_day) +
                       '</b></div></td>' )
            result = result + '<td class="firstevent">' + first_event + '</td></table>'
            result = result + data_str
            result = result + "</td>"
        result = result + "</tr>"
    result = result + """</table>
</TR></TABLE></body>
</html> """
    return result

class TableCell:
    def __init__(self, dt, evs, css_class):
        self.dt = dt
        self.evs = evs
        self.css_class = css_class
    def daystring(self):
        return str(self.dt.day)
    def evstrings(self):
        return [ cgi.escape(ev[0]['SUMMARY']) for ev in self.evs ]

def gen_matrix_of_cells(evs, tdy, nweeks):
    dt_cur_month, dt_cur_week_starts, dt_cur_month_starts, dt_display_starts = specify_dates(tdy, threeweeks=True)

    def running_dt(d, w):
        return dt_display_starts + timedelta(days=d + 7 * w)

    def classstring(cur_dt):
        if cur_dt.month < dt_cur_month:
            return "prevmonth"
        elif cur_dt.month > dt_cur_month:
            return "nextmonth"
        elif cur_dt.day == tdy.day:
            return "today"
        else:
            return "thismonth"

    return [
        [
            TableCell(
                running_dt(d, w),
                mywalk(evs, tdy=running_dt(d, w)),
                classstring(running_dt(d, w))
            )
            for d in range(7)
        ]
        for w in range(nweeks)
    ]


def remove_duplicates(evs):
    encountered = []
    def event_data(ev):
        lns = ev.lines()
        ev_dict = dict((ln.name, ln.value) for ln in lns)
        uid = ev_dict['UID'] if 'UID' in ev_dict.keys() else "NOUID"
        return uid, [(f, ev_dict[f]) for f in HUMAN_FLDS['VEVENT'] if f in ev_dict.keys()]
    for ev in evs:
        evntuid, evntdata = event_data(ev)
        if evntdata in [x[1] for x in encountered] :
            for x in encountered:
                if x[1] == evntdata:
                    try:
                        print(x[0] + " <-- already exists  : " + dict(x[1])['SUMMARY'])
                    except KeyError:
                        print(x[0] + " <-- already exists BUT NO SUMMARY ")
            if evntuid != 'NOUID':
                try:
                    print(evntuid + " <-- will be deleted : " + dict(evntdata)['SUMMARY'])
                    _ , evs = delete_event(evntuid, evs)
                except KeyError:
                    print(evntuid + " <-- will NOT be deleted because SUMMARY not found!")
            else:
                print("--- UID NOT FOUND for the following: ")
                print(evntdata)
        else:
            encountered.append((evntuid, evntdata))
    return evs

class PickleObj():
    def __init__(self, f):
        self.fname = f
        self.uuids = []
        self.mode = None

    def append(self, uuid):
        self.uuids.append(uuid)

    def length(self):
        return len(self.uuids)


def dump_pickle(srlz):
    with open(SERIAL_FILE, "wb") as fh:
        pickle.dump(srlz, fh)


def delete_serial_file():
    remove(SERIAL_FILE)


def show_rgx_matches(evs, rgx, srlz):
    inrange = mywalk(evs, rgx=rgx)
    show_items(inrange, srlz, do_order=False)


def get_all_events(fname):
    with open(fname, 'r') as cfl:
        cl = vobject.readComponents(cfl).next()
    evs = list(cl.components())
    return evs


def download_ics(uri, login, password, filename):
    import pexpect

    def cadaver_error(x):
        system('notify-send "' + x + '"')
        print(str(child))
        sys.exit(x)

    child = pexpect.spawn('cadaver ' + uri)
    try:
        i = child.expect(['.*accept the certificate.*', 'Username:.*'])
    except:
        cadaver_error("Error in downloading calendar: did not ask for certificate or Username")
    if i == 0:
        child.sendline('y')
        try:
            child.expect('Username:.*')
        except:
            cadaver_error("Error in downloading calendar: did not ask for Username")
        child.sendline(login)
    elif i == 1:
        child.sendline(login)
    try:
        child.expect('Password:.*')
    except:
        cadaver_error("Error in downloading calendar: did not ask for Password")
    child.sendline(password)
    try:
        child.expect('dav:/' + login + '/>')
    except:
        cadaver_error("Error in downloading calendar: no prompt")
    child.sendline('get ' + filename)
    try:
        i = child.expect(['Enter local filename for.*', 'dav:/' + login + '/>'])
    except:
        cadaver_error("Error in downloading calendar: at asking for local filename")
    if i == 0:
        child.sendline(filename)
        try:
            child.expect('dav:/' + login + '/>')
        except:
            cadaver_error("Error in downloading calendar: no prompt after specifying local filename")
        child.sendline('quit')
    if i == 1:
        child.sendline('quit')


def command_line_arguments(parser):
    """Invocation"""
    parser.add_option("--date", dest="mydt", default="",
                      help="specific date YYYY-MM-DD", metavar="DATE")
    parser.add_option("--journal", dest="journal",
                      action="store_true", default=False,
                      help="print contents of journal")
    parser.add_option("--todolist", dest="todolist",
                      action="store_true", default=False,
                      help="print contents of todolist")
    parser.add_option("--ics", dest="ics",
                      help="path of ics file", metavar="FILEPATH")
    parser.add_option("--forward", dest="forw", type="int", default=0, help="look days ahead", metavar="DAYS")
    parser.add_option("--offset", dest="offset", type="int", default=0,
                      help="""timezone offset of server relative to where the calendar is used; e.g. when we are using it in California and the server is in NY, then should use OFFSET=3""",
                      metavar="OFFSET")
    parser.add_option("--tt", dest="show_today_and_tomorrow",
                      action="store_true", default=False,
                      help="show events for today and tomorrow")
    parser.add_option("--html", dest="html",
                      action="store_true", default=False,
                      help="generate html calendar")
    parser.add_option("--3w", "-3", dest="threeweeks",
                      action="store_true", default=False,
                      help="when printing the html calendar, only print 3 relevant weeks")
    parser.add_option("--elinks", dest="elinks", action="store_true", default=False,
                      help="view in elinks")
    parser.add_option("--add", "-a", dest="do_add_event", action="store_true", default=False,
                      help="add event")
    parser.add_option("-n", dest="item_no", default=None, type="int",
                      help="pick number N from the previous listing", metavar="N")
    parser.add_option("-d", "--delete", dest="do_delete", default=False, action="store_true",
                      help="""delete items""")
    parser.add_option("-u", "--update", dest="do_update", default=False, action="store_true",
                      help="""update items""")
    parser.add_option("--clone", dest="do_clone", default=False, action="store_true",
                      help="""when updating, do not delete old one""")
    parser.add_option("--full", "-f", dest="pretty_print", default=False, action="store_true",
                      help="""pretty print the complete info about event""")
    parser.add_option("--tz", dest="tz", default="",
                      help="specify timezone", metavar="TZ")
    parser.add_option("--regex", dest="rgx", default=None, help="search summary by regular expression",
                      metavar="REGEX_STR")
    parser.add_option("--2gtd", dest="do_convert_to_gtd", default=False, action="store_true",
                      help="""push items to gtd""")
    parser.add_option("--xml", dest="give_xml", default=False, action="store_true", help="output as XML")
    parser.add_option("--remove-duplicates", dest="remove_dups", default=False, action="store_true", help="remove duplicate events")
    parser.add_option("--get-ics-from-caldav",
                      dest="caldav_yaml",
                      help="get .ics from CalDAV with login data specified in a YAML file, which is a dictionary with the keys: 'uri', 'login', 'password', 'filename' ",
                      metavar="YAML_FILE")

if __name__ == '__main__':
    parser = OptionParser()
    command_line_arguments(parser)
    (options, args) = parser.parse_args()
    if options.caldav_yaml:
        yamfl = open(options.caldav_yaml, 'r')
        y = yaml.safe_load(yamfl)
        yamfl.close()
        download_ics(y['uri'], y['login'], y['password'], y['filename'])
    if options.threeweeks: options.elinks = True
    if options.tz == "":
        tz_here = get_timezone()
    else:
        tz_here = timezone(options.tz)
    if options.journal:
        fname = options.ics if options.ics else DEFAULT_JOURNAL_FILENAME
        mode = 'VJOURNAL'
    elif options.todolist or options.do_convert_to_gtd:
        fname = options.ics if options.ics else DEFAULT_TODOLIST_FILENAME
        mode = 'VTODO'
    else:
        fname = options.ics if options.ics else DEFAULT_FILENAME
        mode = 'VEVENT'

    uid_lst = None
    if options.item_no != None:
        with open(SERIAL_FILE, "rb") as fh:
            serialized_data = pickle.load(fh)
            uid_lst = serialized_data.uuids
            fname = serialized_data.fname
            mode = serialized_data.mode
    else:
        srlz = PickleObj(fname)
        srlz.mode = mode

    if options.do_add_event:
        if options.journal:
            add_vjournal(fname)
        elif options.todolist:
            add_vtodo(fname)
        else:
            add_vevent(fname)
        delete_serial_file()
        sys.exit()
    if options.do_delete:
        if uid_lst:
            events_to_delete, new_evs = delete_event(uid_lst[options.item_no - 1],
                                                     get_events(fname))
            if len(events_to_delete) > 0:
                [x.prettyPrint() for x in events_to_delete]
                print("DELETE THESE ITEMS?");
                inpt = getch()
                if inpt == "y":
                    print("DELETING")
                    write_calendar(build_calendar(new_evs), fname)
            else:
                print("***ERROR: NO UID MATCH***")
        else:
            print("***ERROR: FORGOT TO GIVE -n***")
        delete_serial_file()
        sys.exit()
    if options.do_update:
        if uid_lst:
            events_to_update, remaining_evs = delete_event(uid_lst[options.item_no - 1],
                                                           get_events(fname))
            if not options.do_clone: write_calendar(build_calendar(remaining_evs), fname)
            update_events(events_to_update, read_calendar(fname), fname, ev_type=mode)
        delete_serial_file()
        sys.exit()
    if options.pretty_print:
        if uid_lst:
            evs, _ = delete_event(uid_lst[options.item_no - 1],
                                  get_events(fname))
            for ev in evs: ev.prettyPrint()
        sys.exit()
    if options.mydt == '':
        tdy = datetime.now() - timedelta(hours=options.offset)
    else:
        tdy = parse(options.mydt)
        # tdy = parse(options.mydt).replace(hour=7, minute=0, second=0)
        # tdy = datetime(*(map(lambda u : int(u), options.mydt.split("-"))), 
        #                 hour=7, minute=0, second=0)
    tdy = loose_tz(tdy)

    evs = get_all_events(fname)

    if options.html:
        prnt(gen_html(evs, tdy, threeweeks = options.threeweeks, elinks=options.elinks))
        sys.exit()

    if options.give_xml:
        gen_xml(evs, tdy)
        sys.exit()

    if options.elinks:
        nullfile = open("/dev/null", 'w')
        # w = subprocess.Popen(["elinks", "-dump" , "-dump-color-mode", "3", "-no-references"], 
        #                      stdin = subprocess.PIPE, stdout = subprocess.PIPE, stderr = nullfile)
        w = subprocess.Popen(["lynx", "-stdin", "-dump"],
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=nullfile)
        tail = subprocess.Popen(["tail", "-n", "+2"], stdin=w.stdout)
        print >> w.stdin, gen_html(evs, tdy, srlz, threeweeks = options.threeweeks, elinks=options.elinks)
        w.stdin.close()
        w.stdout.close()
        nullfile.close()
        srlz.mode = 'VEVENT'
        dump_pickle(srlz)
        sys.exit()

    if options.journal:
        if options.rgx:
            show_rgx_matches(evs, options.rgx, srlz)
        else:
            show_journal(evs, srlz)
    if options.todolist:
        if options.rgx:
            show_rgx_matches(evs, options.rgx, srlz)
        else:
            show_todolist(evs, srlz)
    if options.do_convert_to_gtd:
        convert_to_gtd(evs)
    if options.remove_dups:
        new_evs = remove_duplicates(evs)
        print("================================")
        write_calendar(build_calendar(new_evs), fname)
    else:
        inrange = mywalk(evs, tdy=tdy, frw=options.forw, rwnd_hrs=8, rgx=options.rgx)
        if options.show_today_and_tomorrow:
            if inrange:
                print("---- Today: ------------")
                show_items(inrange, srlz)
            inrange_tomorrow = mywalk(evs, tdy=tdy + timedelta(hours=24), frw=options.forw)
            if inrange_tomorrow:
                print("---- Tomorrow: ---------")
                show_items(inrange_tomorrow, srlz)
        else:
            if options.forw > 0:
                blank_days = [(loose_tz(tdy + timedelta(hours=24 * n)), None) for n in range(options.forw + 1)]
            else:
                blank_days = []
            if options.rgx:
                list_items(inrange, srlz)
            else:
                show_items(inrange, srlz, blank_days=blank_days)

    dump_pickle(srlz)

