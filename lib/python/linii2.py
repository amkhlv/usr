#!/usr/bin/env python3

__author__ = "Andrei Mikhailov"
__copyright__ = "Copyright 2014, Andrei Mikhailov"
__license__ = "GPL"

from gi.repository import Gtk, Gdk, GObject
import sqlite3
import sys
import os
import yaml
import functools
import xml.sax.saxutils
import subprocess

class Parameters:
    """
    This class determines some basic settings; those which would be hard to change
    """
    results_batch_size = 12
    tooltip_truncate = 100
    alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    width = 8
    collector_grid_row_spacing = 3
    buttons_grid_row_spacing = 2
    buttons_grid_column_spacing = 2


def emacs_shortcut(w, ev):
    """
    Introduces Emacs keybindings in a widget

    :param Gtk.TextView w: the widget
    :param Gdk.Event ev: the event
    """

    keyname = Gdk.keyval_name(ev.keyval)
    state = ev.state
    buf = w.get_buffer()
    if ( state &  Gdk.ModifierType.CONTROL_MASK ):
        if keyname == "b":
            w.emit("move-cursor", Gtk.MovementStep.LOGICAL_POSITIONS, -1, False)
        if keyname == "f":
            w.emit("move-cursor", Gtk.MovementStep.LOGICAL_POSITIONS, 1, False)
    if ( state &  Gdk.ModifierType.MOD1_MASK ):
        if keyname == "b":
            w.emit("move-cursor", Gtk.MovementStep.WORDS, -1, False)
        if keyname == "f":
            w.emit("move-cursor", Gtk.MovementStep.WORDS, 1, False)

def read_yaml(yaml_filename):
    """
    To read data from a yaml file

    :param str yaml_filename:
    :return: dict
    :rtype: dict
    """
    yamfl = open(yaml_filename, 'r')
    y = yaml.safe_load(yamfl)
    yamfl.close()
    return y

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

class Clmn:
    """
    Information about an sqlite column

    :param dict c: specification of a column
    """
    def __init__(self, c):

        self.name = c['columntitle']
        self.nlines = c['nlines']
        if 'width' in c.keys():
            self.width = c['width']
        else:
            self.width = Parameters.width
        self.hide = c['hide'] if 'hide' in c.keys() else False
        self.do_show = not(self.hide)
        self.balloon = c['balloon'] if 'balloon' in c.keys() else False



class Tbl:
    """
    Information about an sqlite table

    :param dict y: YAML
    """
    def __init__(self, y):
        self.name = y['tablename']
        self.css  = y['style']
        self.columns = [Clmn(c) for c in y['columns']]
        self.primary_key = self.columns[0].name

class Specs:
    """
    Specifications of the database

    :param str yamlfile: YAML file
    """
    def __init__(self, yamlfile):
        y = read_yaml(yamlfile)
        self.dbfile = os.path.expanduser(y['dbfile'])
        self.tables = [Tbl(t) for t in y['tables']]
        self.css = os.path.expanduser(y['css'])

def get_sqlite_connection(specs):
    """
    Get sqlite connection

    :param Specs specs: see :class:`Specs`
    :return: sqlite3.Connection
    :rtype: sqlite3.Connection
    """
    conn = sqlite3.connect(specs.dbfile)
    conn.row_factory = sqlite3.Row
    def hastags(self, x, y):
        """
        Checks if y matches the tag pattern x,

        :param str x: tag pattern, an example of a tag pattern is 'aaa,bbb|^ccc'
        :param str y: string to match
        """
        if not(y): y=""
        xs = x.split("|")
        xss = [u.split(",") for u in xs]
        ys = y.split(",")
        def myMatch1S(pat, strs):
            if pat[0] == "^":
                return functools.reduce(
                    lambda p, q: ( p and q ),
                    [ pat[1:] != str_ for str_ in strs],
                    True)
            else:
                return functools.reduce(
                    lambda p, q: ( p or q ),
                    [ pat == str_ for str_ in strs],
                    False)
        def myMatchSS(pats, strs):
            return functools.reduce(
                lambda p, q: (p and q),
                [ myMatch1S(pat, strs) for pat in pats ],
                True)
        answer = functools.reduce(
            lambda p, q: (p or q),
            [ myMatchSS(u, ys) for u in xss],
            False)
        if (answer):
            return 1
        else:
            return 0
    def eqsql(x, y):
        if not(x) and not(y): return True
        elif not(x) and y == "" : return True
        elif x == "" and not(y) : return True
        else: return x == y
    conn.create_function("tags", 2, hastags)
    conn.create_function("eqli", 2, eqsql)
    return conn

def sqlite_execute(specs, query, data):
    """
    Execute an sqlite statement and return the resulting list of results

    :param Specs specs: see :class:`Specs`
    :param str query:
    :param tuple data:
    :return: list
    :rtype: list
    """
    if data: assert isinstance(data, tuple)
    conn = get_sqlite_connection(specs)
    c = conn.cursor()
    if data:
        c.execute(query, data)
    else:
        c.execute(query)
    result = c.fetchall()
    c.close()
    conn.commit()
    conn.close()
    return result

def sqlite_insert_values(specs, tbl, data):
    """
    Insert values into the table

    :param Specs specs: see :class:`Specs`
    :param Tbl tbl: see :class:`Tbl`
    :param dict[str,str] data:
    """
    qmarks = " (" + ",".join(list("?" * len(data.keys()))) + ") "
    query = "INSERT INTO " + tbl.name + " (" + ",".join(data.keys()) + ") " + " VALUES " + qmarks
    sqlite_execute(specs, query, tuple([data[k] for k in data.keys()]))

def sqlite_delete_values(specs, tbl, data):
    """
    Delete values from the table

    :param Specs specs: see :class:`Specs`
    :param Tbl tbl: see :class:`Tbl`
    :param dict[str,str] data: this is a dictionary str -> str
    """
    query = "DELETE FROM " + tbl.name + " WHERE " + " AND ".join(["eqli(" + k + ",?)" for k in data.keys()])
    print(query)
    print(" ; ".join([(data[k] or "") for k in data.keys()]))
    sqlite_execute(specs, query, tuple([data[k] for k in data.keys()]))

def sqlite_update_values(specs, tbl, old_data, new_data):
    """
    Update values in Sqlite table

    :param Specs specs: see :class:`Specs`
    :param Tbl tbl: see :class:`Tbl`
    :param dict[str,str] old_data:
    :param dict[str,str] new_data:
    """
    assignments = ", ".join([name + " = ? " for name in new_data.keys()])
    conditions = " AND ".join(["eqli(" + k + ",?)" for k in old_data.keys()])
    test_query = "SELECT * FROM " + tbl.name + " WHERE " + conditions
    test = sqlite_execute(
        specs,
        test_query,
        tuple([old_data[k] for k in old_data.keys()])
    )
    if test:
        print("--- About to update the following record: \n")
        for x in test:
            for k in x.keys():
                print(k + ": " + (x[k] or ""))
    else:
        Alert("RECORD NOT FOUND").initUI()
    query = "UPDATE " + tbl.name + " SET " + assignments + " WHERE " + conditions
    sqlite_execute(
        specs,
        query,
        tuple([new_data[k] for k in new_data.keys()] + [old_data[k] for k in old_data.keys()])
    )

class Env:
    def __init__(self,specs):
        self.specs = specs
        self.connection = get_sqlite_connection(specs)


def truncate(s, length, encoding='utf-8'):
    if not s : return ""
    if len(s) > length :
        return "".join(list(s)[:length]) + "\N{HORIZONTAL ELLIPSIS}"
    else:
        return s

class Alert(Gtk.Window):
    def __init__(self, message):
        """
        Alert message

        :param str message: the alert message
        """
        print("====================================")
        print(message + "\n")
        print("====================================")
        self.message=message
        Gtk.Window.__init__(self)

    def initUI(self):
        self.box = Gtk.HBox()
        self.add(self.box)
        self.lbl = Gtk.Label()
        self.lbl.set_name("AlertLabel")
        self.lbl.set_markup("<b>" + self.message + "</b>")
        self.box.add(self.lbl)
        self.show_all()

class ColumnHighlightToggles:
    """
    Row of column labels (part of a Buttons onject)

    :param Tbl tbl: see :class:`Tbl`
    :param ButtonsState state: see :class:`ButtonsState`
    :param Gtk.Grid grid:
    """
    def __init__(self, tbl, state, grid):
        self.toggle = {}
        clmns_to_show = [ c for c in tbl.columns if c.name in state.columns_to_show ]
        self.topleft_hbox = Gtk.HBox()
        self.topleft_hbox.pack_start(Gtk.VBox(),False,False,2)
        self.topleft_label = Gtk.Label(name = "ResultsNumberLabel")
        self.topleft_label.set_markup("<big>" + str(state.index + 1) + "</big>")
        self.topleft_hbox.add(self.topleft_label)
        grid.add(self.topleft_hbox)
        prev_toggle  = None
        for c in clmns_to_show :
            self.toggle[c.name] = Gtk.ToggleButton(label = c.name)
            if c.name in state.highlighted: self.toggle[c.name].set_active(True)
            if prev_toggle:
                grid.attach_next_to(self.toggle[c.name], prev_toggle, Gtk.PositionType.RIGHT, 1, 1)
            else:
                grid.attach_next_to(self.toggle[c.name], self.topleft_hbox, Gtk.PositionType.RIGHT, 1, 1)
            prev_toggle = self.toggle[c.name]
    def get_toggles(self):
        """
        gets the dictionary of toggle buttons, of the type: `str` -> `Gtk.ToggleButton`

        :returns: dict[str, Gtk.ToggleButton]
        :rtype: dict[str, Gtk.ToggleButton]
        """
        return self.toggle

class Row:
    """
    Row of item labels (part of a Buttons object)

    :param Specs specs: see :class:`Specs`
    :param Tbl tbl: see :class:`Tbl`
    :param Results results:
    :param sqlite3.Row row_of_items:
    :param Gtk.Grid grid:
    :param Gtk.Widget prev: previous
    :param int n:
    """
    def __init__(self, specs, tbl, results, row_of_items, grid, prev, n):
        self.specs = specs
        self.tbl = tbl
        self.results = results
        self.row_of_items = row_of_items
        self.aux_hbox = Gtk.HBox()
        self.aux_hbox.pack_start(Gtk.VBox(),False,False,2)
        self.item_button = Gtk.Button(
            "[F" + str(n) + "] " + row_of_items[0],
            name="RowItemButton"
            )
        self.item_button.connect("clicked", self.update_fn)
        self.aux_hbox.add(self.item_button)
        self.labels = {}; self.bttns = {};
        grid.attach_next_to(self.aux_hbox, prev, Gtk.PositionType.BOTTOM, 1, 1)
        j = 0; prev_bttn = None
        for k in row_of_items.keys()[1:] :
            item = row_of_items[k]
            self.bttns[k] = Gtk.Button()
            self.bttns[k].set_name("ItemButton")
            def mk_copy_fn(txt):
                def copy_fn(self):
                    xsel = subprocess.Popen(["xsel", "-i"], stdin=subprocess.PIPE)
                    xsel.communicate(txt.encode())
                return copy_fn
            self.bttns[k].connect("clicked", mk_copy_fn(item))
            self.labels[k] = Gtk.Label()
            self.labels[k].set_name("ItemLabel" + str(j % 7))
            truncate_to_length = [clmn for clmn in tbl.columns if clmn.name == k][0].width
            self.labels[k].set_markup(
                xml.sax.saxutils.escape(truncate(item, truncate_to_length))
            )
            self.labels[k].set_tooltip_markup(xml.sax.saxutils.escape(truncate(item, Parameters.tooltip_truncate)))
            self.labels[k].set_markup(
                xml.sax.saxutils.escape(truncate(item, truncate_to_length))
            )
            self.bttns[k].add(self.labels[k])
            if prev_bttn:
                grid.attach_next_to(self.bttns[k], prev_bttn, Gtk.PositionType.RIGHT, 1, 1)
            else:
                grid.attach_next_to(self.bttns[k], self.aux_hbox, Gtk.PositionType.RIGHT, 1, 1)
            prev_bttn = self.bttns[k]
            j = j + 1
        grid.attach_next_to(Gtk.VBox(), prev_bttn, Gtk.PositionType.RIGHT, 1, 1)
    def update_fn(self, button):
        p = Prefill(dict(zip(list(self.row_of_items.keys()), self.row_of_items)), ['UPDATE', 'READONLY'])
        collector = CollectorGUI(self.specs, self.tbl, p, self.results)
        collector.initUI()

class Prefill:
    """
    Prefill object

    :param dict data:
    :param list flags:
    """
    def __init__(self, data, flags):
        self.data = data
        self.flags = flags
        self.columns = data.keys()
        allowed_flags = ['UPDATE', 'DELETE', 'READONLY']
        for f in flags: assert f in allowed_flags
    def empty(columns, flags):
        empty_data = dict([(c,"") for c in columns])
        return Prefill(empty_data, flags)

class CollectorGUI(Gtk.Window):
    def __init__(self, specs, table, prefill, results = None):
        """
        Collector window

        :param Specs specs: see :class:`Specs`
        :param Tbl table: see :class:`Tbl`
        :param Prefill prefill: see :class:`Prefill`
        :param Results results: see :class:`Results`
        """
        self.specs = specs
        self.table = table
        self.prefill = prefill
        self.results = results
        self.label = {}
        self.text_entry = {}
        self.text_buffer = {}
        self.scrollwin = {}
        Gtk.Window.__init__(self)
    def initUI(self):
        vbox = wrap(Gtk.VBox(),
                    name = "CollectorBoxUpdate" if 'UPDATE' in self.prefill.flags else "CollectorBoxCollect",
                    collector=self)
        self.add(vbox)
        tophbox = Gtk.HBox()
        bottomhbox = Gtk.HBox()
        grid = Gtk.Grid()
        grid.set_row_spacing(Parameters.collector_grid_row_spacing)
        vbox.add(tophbox)
        vbox.add(grid)
        vbox.add(bottomhbox)
        j = 0; prev_label = None
        for column in self.table.columns:
            if column.name not in self.prefill.columns:
                continue
            self.label[column] = Gtk.Label()
            escaped_column_name = xml.sax.saxutils.escape(column.name)
            decorated_column_name = "<b>" + escaped_column_name +"</b>" if column.balloon else escaped_column_name
            self.label[column].set_markup("F" + str(1 + (j % 12)) + " " + decorated_column_name + " ")
            if column.balloon: self.label[column].set_tooltip_markup(xml.sax.saxutils.escape(column.balloon))
            if prev_label:
                grid.attach_next_to(self.label[column], prev_label, Gtk.PositionType.BOTTOM, 1, 1)
            else:
                grid.add(self.label[column])
            self.text_entry[column] = Gtk.TextView(name = "TextViewPrimaryKey" if j == 0 else "TextViewColumn")
            self.text_entry[column].set_editable(not('READONLY' in self.prefill.flags))
            self.text_entry[column].connect("data-entered", self.collect_or_update_fn)
            text_style = self.text_entry[column].get_style_context()
            text_font  = text_style.get_font(Gtk.StateFlags.NORMAL)
            self.text_buffer[column] = self.text_entry[column].get_buffer()
            text = self.prefill.data[column.name]
            self.text_buffer[column].set_text(text if text else "")
            self.scrollwin[column] = Gtk.ScrolledWindow(
                name = "TextViewPrimaryKeyScrollWin" if j == 0 else "TextViewColumnScrollWin" )
            print("Size=" + str(text_font.get_size()))
            self.scrollwin[column].set_min_content_width(50 * (text_font.get_size()/1024))
            self.scrollwin[column].set_min_content_height((column.nlines * text_font.get_size()/1024))
            self.scrollwin[column].add(self.text_entry[column])
            grid.attach_next_to(self.scrollwin[column], self.label[column], Gtk.PositionType.RIGHT, 1, 1)
            prev_label = self.label[column]
            j = j + 1
        unlock_button = Gtk.Button("unlock")
        unlock_button.connect("clicked", self.unlock_fn)
        delete_button = Gtk.Button("delete")
        delete_button.connect("clicked", self.confirm_delete_fn)
        confirm_delete_label = Gtk.Label()
        confirm_delete_label.set_markup("Really want to delete? ")
        no_dontdelete_button = Gtk.Button("NO")
        no_dontdelete_button.set_name("NoDontDeleteButton")
        no_dontdelete_button.connect("clicked", self.no_dontdelete_fn)
        yes_dodelete_button  = Gtk.Button("YES")
        yes_dodelete_button.set_name("YesDoDeleteButton")
        yes_dodelete_button.connect("clicked", self.yes_dodelete_fn)
        collect_button = Gtk.Button("update" if 'UPDATE' in self.prefill.flags else "collect")
        collect_button.connect("clicked", self.update_fn if 'UPDATE' in self.prefill.flags else self.collect_fn)
        if 'DELETE' in self.prefill.flags:
            bottomhbox.add(confirm_delete_label)
            bottomhbox.add(no_dontdelete_button)
            bottomhbox.add(yes_dodelete_button)
        elif 'READONLY' in self.prefill.flags:
            bottomhbox.add(unlock_button)
        else:
            bottomhbox.add(delete_button)
            bottomhbox.add(collect_button)
        self.show_all()
    def get_text_entries(self):
        """
        get dictionary of text entries, of the type :class:`Clmn` -> :class:`Gtk.TextView`

        :return: dict from :class:`Clmn` to :class:`Gtk.TextView`
        :rtype: dict[Clmn,Gtk.TextView]
        """
        return self.text_entry
    def get_data_dict(self):
        """
        get dictionary of data, of the type `str` -> `str`

        :return: dict[str,str]
        :rtype: dict[str,str]
        """
        data_dict = {}
        for column in self.table.columns:
            if column in self.text_buffer.keys():
                buffer = self.text_buffer[column]
                data_dict[column.name] = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
        return data_dict
    def unlock_fn(self, button):
        """
        to unlock

        :param Gtk.Button button: put in anything, it is not used but the argument has to be present
        """
        self.prefill.flags = ['UPDATE']
        new_collector = CollectorGUI(self.specs, self.table, self.prefill, self.results)
        self.destroy()
        new_collector.initUI()
    def confirm_delete_fn(self, button):
        """
        to intitiate the confirmation procedure for DELETE

        :param Gtk.Button button: put in anything, it is not used but the argument has to be present
        """
        self.prefill.flags = ['DELETE']
        new_collector = CollectorGUI(self.specs, self.table, self.prefill, self.results)
        self.destroy()
        new_collector.initUI()
    def yes_dodelete_fn(self, button):
        """
        to confirm: YES, do delete!

        :param Gtk.Button button: put in anything, it is not used but the argument has to be present
        """
        data_dict = self.get_data_dict()
        sqlite_delete_values(self.specs, self.table, data_dict)
        if self.results: self.results.refresh(None)
        self.destroy()
    def no_dontdelete_fn(self, button):
        """
        to say: NO, dont delete!

        :param Gtk.Button button: put in anything, it is not used but the argument has to be present
        """
        self.prefill.flags = ['UPDATE', 'READONLY']
        new_collector = CollectorGUI(self.specs, self.table, self.prefill, self.results)
        self.destroy()
        new_collector.initUI()
    def collect_fn(self, button):
        """
        to collect new data

        :param Gtk.Button button: put in anything, it is not used but the argument has to be present
        """
        data_dict = self.get_data_dict()
        sqlite_insert_values(self.specs, self.table, data_dict)
        if self.results: self.results.refresh(None)
        self.destroy()
    def update_fn(self, button):
        """
        to update data

        :param Gtk.Button button: put in anything, it is not used but the argument has to be present
        """
        data_dict = self.get_data_dict()
        print(self.prefill.data[self.table.primary_key])
        print(data_dict[self.table.primary_key])
        if self.prefill.data[self.table.primary_key] == data_dict[self.table.primary_key]:
            sqlite_update_values(self.specs, self.table, self.prefill.data, data_dict)
        else:
            sqlite_insert_values(self.specs, self.table, data_dict)
        if self.results: self.results.refresh(None)
        self.destroy()
    def collect_or_update_fn(self,button):
        """
        either collect or update, depending on flags

        :param Gtk.Button button: anything (unused)
        """
        if 'UPDATE' in self.prefill.flags :
            self.update_fn(button)
        else:
            self.collect_fn(button)
        if self.results: self.results.refresh(None)

class ButtonsState:
    """
    Information about the state of the Buttons widget

    :param list[Row] rows: listof :class:`Row`
    :param set[str] highlighted: setof `str`
    :param Results results:
    """
    def __init__(self, rows, highlighted, results):
        self.columns_to_show = results.columns_to_show
        self.rows = rows
        self.highlighted = highlighted
        self.index = results.context.index
        self.results = results

class Buttons(Gtk.VBox):
    def __init__(self, specs, table, state):
        """
        Buttons

        :param Specs specs: see :class:`Specs`
        :param Tbl table: see :class:`Tbl`
        :param ButtonsState state: see :class:`ButtonsState`
        """
        self.specs = specs
        self.table = table
        self.state = state
        self.tophbox = Gtk.HBox()
        self.grid = Gtk.Grid()
        self.grid.set_row_spacing(Parameters.buttons_grid_row_spacing)
        self.grid.set_column_spacing(Parameters.buttons_grid_column_spacing)
        Gtk.VBox.__init__(self)
        self.set_name("ButtonsVBox")
        self.add(self.tophbox)
        self.add(self.grid)
        self.pack_end(Gtk.VBox(),False,False,2)
        self.column_highlight_toggles = ColumnHighlightToggles(self.table, self.state, self.grid)
        self.displayed_rows = []
        def highlight_pre_fn(clmn: str):
            def highlight_fn(b):
                if not(b.get_active()):
                    self.state.highlighted.remove(clmn)
                    for r in self.displayed_rows:
                        r.labels[clmn].set_name(
                            "ItemLabel" + str(self.state.columns_to_show.index(clmn) % 7)
                        )
                else:
                    self.state.highlighted.add(clmn)
                    for r in self.displayed_rows:
                        r.labels[clmn].set_name("HighlightedLabel")
            return highlight_fn
        prev = self.column_highlight_toggles.topleft_hbox
        for row in self.state.rows:
            r = Row(self.specs, self.table, self.state.results, row, self.grid, prev, 1 + len(self.displayed_rows))
            for k in r.labels.keys():
                if k in self.state.highlighted:
                    r.labels[k].set_name("HighlightedLabel")
            self.displayed_rows.append(r)
            prev = r.aux_hbox
        i = 0
        for clmn in self.state.columns_to_show:
            self.column_highlight_toggles.get_toggles()[clmn].connect("toggled", highlight_pre_fn(clmn))
            i = i + 1
    def get_column_highlight_toggles(self):
        """
        gets column highlight toggle buttons, see :class:`ColumnHighlightToggles`

        :returns: :class:`ColumnHighlightToggles`
        :rtype: ColumnHighlightToggles
        """
        return self.column_highlight_toggles

class Context:
    """
    For the widget to know about it parents etc

    :param Commander commander: see :class:`Commander`
    :param Gtk.Widget escape_to:
    :param Gtk.Window mainwin:
    """

    def __init__(self, commander, escape_to, mainwin, index):
        self.commander = commander
        self.escape_to = escape_to
        self.mainwin = mainwin
        self.index = index

class Results(Gtk.VBox):
    def __init__(self, specs, table, context, columns_to_show, query, data):
        """
        Show results

        :param Specs specs: see :class:`Specs`
        :param Tbl table: see :class:`Tbl`
        :param Context context: see :class:`Context`
        :param list columns_to_show: `list` of `str`
        :param str query:
        :param tuple data:
        """
        self.specs = specs
        self.table = table
        self.context = context
        self.columns_to_show = columns_to_show
        self.query = query
        self.data = data
        self.buttons = None
        self.batch_no = 0
        Gtk.VBox.__init__(self)
        self.set_can_focus(True)
        self.connect("focus-to-commander", self.context.commander.focus_to_top)
        wrap(self, "Results", results = self)
        self.initWidget()
    def initWidget(self):
        self.rows = sqlite_execute(self.specs, self.query, self.data)
        self.tophbox = Gtk.HBox()
        self.tophbox.set_name("ResultsTopHBox")
        self.go_prev_batch_button = Gtk.Button("\N{LEFTWARDS WHITE ARROW}<h>" if self.batch_no > 0 else "-------")
        self.go_prev_batch_button.set_name("GotoPrevBatchButton")
        self.go_next_batch_button = Gtk.Button("<l>\N{RIGHTWARDS WHITE ARROW}" if self.exist_more_batches() else "-------")
        self.go_next_batch_button.set_name("GotoNextBatchButton")
        if self.batch_no > 0: self.go_prev_batch_button.connect("clicked", self.go_prev_batch_fn)
        if self.exist_more_batches(): self.go_next_batch_button.connect("clicked", self.go_next_batch_fn)
        self.tophbox.pack_start(Gtk.VBox(),False,False,2)
        self.tophbox.add(self.go_prev_batch_button)
        self.tophbox.add(self.go_next_batch_button)
        self.refresh_btn = Gtk.Button("refresh<r>")
        self.refresh_btn.connect("clicked", self.refresh)
        self.tophbox.add(self.refresh_btn)
        self.new_btn = Gtk.Button("new<n>")
        self.new_btn.connect("clicked", self.collect_new)
        self.tophbox.add(self.new_btn)
        self.destroy_btn = Gtk.Button("destroy<q>")
        self.destroy_btn.connect("clicked", self.destroy_fn)
        self.tophbox.add(self.destroy_btn)
        self.tophbox.pack_end(Gtk.VBox(),False,False,2)
        batch_size = Parameters.results_batch_size
        self.buttons = Buttons(
            self.specs,
            self.table,
            ButtonsState(
                self.rows[ self.batch_no * batch_size: (self.batch_no + 1) * batch_size ],
                self.buttons.state.highlighted if self.buttons else set(),
                self
            )
        )
        self.top_empty_vbox = Gtk.VBox()
        self.pack_start(self.top_empty_vbox, False, False, 3)
        self.add(self.tophbox)
        self.add(self.buttons)
        self.show_all()
        self.context.mainwin.resize(1,1)
    def cleanup(self):
        """
        cleanup

        :return:
        """
        self.top_empty_vbox.destroy()
        self.tophbox.destroy()
        self.buttons.destroy()
    def get_batch_no(self):
        """
        get batch number

        :return: int
        :rtype: int
        """
    def exist_more_batches(self):
        """
        check if there are more batches

        :return: bool
        :rtype: bool
        """
        return self.batch_no < int((len(self.rows) - 1)/Parameters.results_batch_size)
    def get_buttons(self) -> Buttons:
        """
        get buttons, see :class:`Buttons`

        :returns: :class:`Buttons`
        :rtype: Buttons
        """
        return self.buttons
    def collect_new(self,b):
        """
        Collect new record

        :param Gtk.Button b:
        """
        prefill = Prefill.empty([c.name for c in self.table.columns if c.do_show], [])
        collector = CollectorGUI(self.specs, self.table, prefill, self)
        collector.initUI()
    def refresh(self, b):
        """
        Refresh

        :param Gtk.Button b:
        """
        self.cleanup()
        self.initWidget()
    def go_prev_batch_fn(self, b):
        """
        Goto previous 12

        :param Gtk.Button b:
        """
        self.batch_no = self.batch_no - 1
        self.refresh(b)
    def go_next_batch_fn(self, b):
        """
        Goto next 12

        :param Gtk.Button b:
        """
        self.batch_no = self.batch_no + 1
        self.refresh(b)
    def destroy_fn(self, b):
        """
        Destroy itself

        :param Gtk.Button b:
        """
        self.destroy()
        self.context.commander.results.remove(self)
        j = 1
        for r in self.context.commander.results:
            assert isinstance(r, Results)
            r.get_buttons().get_column_highlight_toggles().topleft_label.set_markup("<big>" + str(j) + "</big>")
            j=j+1
        self.context.commander.top.grab_focus()
        self.context.mainwin.resize(1,1)

class Commander(Gtk.VBox):
    def __init__(self, specs, table, mainwin):
        """
        Topmost widget: the commandline

        :param Specs specs: see :class:`Specs`
        :param Tbl table: see :class:`Tbl`
        :param Gtk.Window mainwin:
        """
        self.specs = specs
        self.table = table
        self.mainwin = mainwin
        Gtk.VBox.__init__(self, spacing=1)
        self.set_name("CommanderMain")
        self.cmdline = wrap(Gtk.TextView(), "CommandLine", commander=self)
        self.results = []
    def initWidget(self):
        self.top = wrap(Gtk.VBox(), "CommanderTop", commander=self)
        self.top.set_can_focus(True)
        self.cmdline_box = wrap(Gtk.VBox(), "CommanderCmdlineBox", commander=self)
        self.aux_hbox = Gtk.HBox()
        self.aux_hbox.pack_start(Gtk.VBox(),False,False,2)
        self.aux_hbox.add(self.cmdline_box)
        self.aux_hbox.pack_end(Gtk.VBox(),False,False,2)
        self.toggles_box = wrap(Gtk.HBox(spacing=1), "CommanderTogglesBox", commander=self)
        self.bottom = wrap(Gtk.VBox(spacing=1), "CommanderBottom", commander=self)
        self.add(self.top)
        self.add(self.bottom)
        self.top.pack_start(Gtk.VBox(),False,False,2)
        self.top.add(self.aux_hbox)
        self.top.add(self.toggles_box)
        self.top.pack_end(Gtk.VBox(),False, False, 3)
        self.toggle = []
        self.primary_key_label = wrap(Gtk.Label(), "CommanderPrimaryKeyLabel", commander=self)
        self.primary_key_label.set_markup(xml.sax.saxutils.escape(self.table.primary_key))
        self.toggles_box.pack_start(Gtk.VBox(),False,False,2)
        self.toggles_box.add(self.primary_key_label)
        for j in range(1,len(self.table.columns)):
            self.toggle.append(
                wrap(
                    Gtk.ToggleButton(label = self.table.columns[j].name),
                    "CommanderToggleButton",
                    commander=self
                )
            )
            self.toggles_box.add(self.toggle[j-1])
        self.toggles_box.pack_end(Gtk.VBox(),False,False,2)
        self.cmd_buffer = self.cmdline.get_buffer()
        self.rows_button = wrap(Gtk.Button("rows"), "RowsButton", commander=self)
        self.rows_button.connect("clicked", self.show_results)
        self.cmdline_box.add(self.cmdline)
        self.cmdline_box.add(self.rows_button)

        self.show_all()
    def get_toggles(self):
        """
        get list of `Gtk.ToggleButton`, which determines which columns are shown in results

        :return: list
        :rtype: list
        """
        return self.toggle
    def focus_to_commandline(self, b=None):
        """
        Focus the commandline

        :param b:
        :return:
        """
        self.cmdline.grab_focus()
    def focus_to_top(self, b=None):
        """
        Focus the top of the commander

        :param b:
        :return:
        """
        self.top.grab_focus()
    def show_results(self,b = None):
        """
        show in the bottom panel the results of the query typed in the text area
        """
        columns_selected = [
            self.table.columns[j].name
            for j in range(1,len(self.table.columns)) if self.toggle[j-1].get_active()
        ]
        what_to_select = ",".join([self.table.primary_key] + columns_selected) if columns_selected else "*"
        columns_to_show = columns_selected if columns_selected else [c.name for c in self.table.columns][1:]
        text_in_cmd_buffer = self.cmd_buffer.get_text(
            self.cmd_buffer.get_start_iter(),
            self.cmd_buffer.get_end_iter(),
            True)
        if text_in_cmd_buffer:
            query = "SELECT " + what_to_select + " FROM " + self.table.name + " WHERE " + text_in_cmd_buffer
        else:
            query = "SELECT " + what_to_select + " FROM " + self.table.name
        print(query)
        r = Results(self.specs,
                    self.table,
                    Context(self, self, self.mainwin, len(self.results)),
                    columns_to_show,
                    query,
                    None)
        self.bottom.add(r)
        self.results.append(r)
    def get_results(self,b = None):
        """
        get the list of all the results shown in the bottom panel, return type is `list` of :class:`Results`

        :param b:
        :return: list of :class:`Results`
        :rtype: list[Results]
        """
        return self.results

class TableChooser(Gtk.Window):
    def __init__(self, specs, choosen_tables):
        """
        Window to choose the table

        :param Specs specs: see :class:`Specs`
        :param list choosen_tables:
        """
        self.specs = specs
        self.choosen_tables = choosen_tables
        Gtk.Window.__init__(self)
        register_css(specs.css)
        vbox = wrap(Gtk.VBox(), "TableChooserVBox", chooser=self)
        self.add(vbox)
        l = {}
        self.hinted_tables = dict(zip(Parameters.alphabet, specs.tables))
        for p in zip(Parameters.alphabet, specs.tables):
            k = p[0]
            l[k] = Gtk.Label()
            l[k].set_markup("<b>" + k + "</b>: " + self.hinted_tables[k].name )
            vbox.add(l[k])
        self.show_all()

def table_chooser(specs):
    """
    Choose a table (of the class :class:`Tbl`) from the list

    :param Specs specs: see :class:`Specs`
    :returns: :class:`Tbl`
    :rtype: Tbl
    """
    choosen_tables = []
    win = TableChooser(specs, choosen_tables)
    win.connect("delete-event", Gtk.main_quit) # this is to abort the execution of the program
    Gtk.main()
    return choosen_tables[0]

def wrap(widget, name, commander = None, collector = None, results = None, chooser = None):
    """
    This is mostly to set up keyboard shortcuts, and also the name of the widget

    :param Gtk.Widget widget:
    :param str name:
    :param Commander commander: see :class:`Commander`
    :param CollectorGUI collector: see :class:`CollectorGUI`
    :param Results results: see :class:`Results`
    :param Gtk.Window chooser:
    :return: Gtk.Widget
    :rtype: Gtk.Widget
    """
    widget.set_name(name)
    if name == "CommanderWindow" or name == "CommanderTogglesBox":
        widget.connect("data-entered", commander.show_results)
        widget.connect("request-for-commandline", commander.focus_to_commandline)
    if name == "CommandLine" or name == "CommanderTogglesBox":
        widget.connect("data-entered", commander.show_results)
        widget.connect("focus-to-commander", commander.focus_to_top)
    if name == "ResultsTopHBox":
        widget.connect("focus-to-commander", results.context.commander.focus_to_top)
    def on_key_press_event(widget, event):
        keyname = Gdk.keyval_name(event.keyval)
        state = event.state
        print("Key %s (%d) was pressed in %s" , (keyname, event.keyval, widget.get_name()))
        if chooser:
            for k in chooser.hinted_tables.keys():
                if keyname == k:
                    chooser.choosen_tables.append(chooser.hinted_tables[k])
                    widget.destroy()
                    Gtk.main_quit()
        elif commander:
            if widget.get_name() == "CommandLine": emacs_shortcut(widget,event)
            if widget.get_name() == "CommanderTop":
                for j in range(1,13):
                    if keyname == "F" + str(j):
                        commander.get_toggles()[j-1].set_active(not(commander.get_toggles()[j-1].get_active()))
                    else:
                        if keyname == str(j): commander.results[j-1].grab_focus()
        elif results:
            cmdr_of_results = results.context.commander
            if keyname == "q":
                results.destroy_fn(None)
                cmdr_of_results.top.grab_focus()
            elif keyname == "n" or keyname == "r":
                if keyname == "n":
                    results.collect_new(None)
                rslts = cmdr_of_results.get_results()
                for rslt in rslts:
                    rslt.refresh(None)
                cmdr_of_results.top.grab_focus()
            elif keyname == "h":
                if results.batch_no > 0 : results.go_prev_batch_fn(None)
            elif keyname == "l":
                if results.exist_more_batches() : results.go_next_batch_fn(None)
            else:
                for j in range(1,13):
                    if keyname == str(j):
                        rslts = results.context.commander.results
                        if (j - 1) < len(rslts): rslts[j-1].grab_focus()
                    elif keyname == "F" + str(j):
                        r = results.get_buttons().displayed_rows[j-1]
                        r.update_fn(r.item_button)
        elif collector:
            for j in range(1,13):
                if keyname == "F" + str(j):
                    ctrl = 12 if ( state &  Gdk.ModifierType.CONTROL_MASK ) else 0
                    collector.get_text_entries()[collector.table.columns[j-1 + ctrl]].grab_focus()
    def on_key_release_event(widget, event):
        keyname = Gdk.keyval_name(event.keyval)
    if results: mainwin = results
    elif chooser: mainwin = chooser
    else : mainwin = widget
    mainwin.connect("key_press_event", on_key_press_event)
    mainwin.connect("key_release_event", on_key_release_event)
    return widget

def configure_signals():
    """
    Configure GTK signals

    :return:
    """
    GObject.type_register(Commander)
    GObject.signal_new("data-entered", Gtk.TextView, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())
    GObject.signal_new("data-entered", Commander, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())
    GObject.signal_new("data-entered", Gtk.HBox, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())
    GObject.signal_new("request-for-commandline", Commander, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())
    GObject.signal_new("request-for-commandline", Gtk.HBox, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())
    GObject.signal_new("focus-to-commander", Gtk.TextView, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())
    GObject.signal_new("focus-to-commander", Gtk.HBox, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())
    GObject.signal_new("focus-to-commander", Gtk.VBox, GObject.SIGNAL_ACTION, GObject.TYPE_NONE, ())

if __name__ == '__main__':
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("-y", "--yaml", dest="yaml_file", metavar="YAML_FILE", help="""specify .yaml file""")
    parser.add_option("-t", "--table", dest="table", metavar="TABLE", help = """specify which table""")
    (options, args) = parser.parse_args()
    myspecs = Specs(options.yaml_file)
    if options.table:
        mytable = [t for t in myspecs.tables if t.name == options.table][0]
    else:
        mytable = table_chooser(myspecs)
    configure_signals()
    win = Gtk.Window(name = "MainWindow")
    win.connect("delete-event", Gtk.main_quit)
    register_css(mytable.css)
    cmdr1 = Commander(myspecs, mytable, win)
    cmdr = wrap(cmdr1, "CommanderWindow", commander=cmdr1)
    cmdr.grab_focus()
    win.add(cmdr)
    cmdr.initWidget()
    win.show_all()
    Gtk.main()
