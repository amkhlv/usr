#!/usr/bin/env python3

__author__ = "Andrei Mikhailov"
__copyright__ = "Copyright 2014, Andrei Mikhailov"
__license__ = "GPL"

from gi.repository import Gtk, Gdk
import sqlite3
import sys
import os
import yaml
import functools
import xml.sax.saxutils

class Parameters:
    results_batch_size = 12
    tooltip_truncate = 100
    alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
                'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    width = 8

def read_yaml(yaml_filename):
    """
    To read data from a yaml file
    @type yaml_filename: str
    """
    yamfl = open(yaml_filename, 'r')
    y = yaml.safe_load(yamfl)
    yamfl.close()
    return y

def register_css(css_filename):
    """
    To register a CSS style file
    @type css_filename: str
    """
    style_provider = Gtk.CssProvider()

    css = open(os.path.expanduser(css_filename), 'rb') # rb needed for python 3 support
    css_data = css.read()
    css.close()

    style_provider.load_from_data(css_data)

    Gtk.StyleContext.add_provider_for_screen(
        Gdk.Screen.get_default(),
        style_provider,
        Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
    )

class Clmn:
    def __init__(self, c):
        """
        Information about an sqlite column
        @type c: dict
        """
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
    def __init__(self, y):
        """
        Information about an sqlite table
        @type y: dict
        """
        self.name = y['tablename']
        self.css  = y['style']
        self.columns = [Clmn(c) for c in y['columns']]
        self.primary_key = self.columns[0].name

class Specs:
    def __init__(self, yamlfile):
        y = read_yaml(yamlfile)
        self.dbfile = os.path.expanduser(y['dbfile'])
        self.tables = [Tbl(t) for t in y['tables']]
        self.css = os.path.expanduser(y['css'])

def get_sqlite_connection(specs):
    """
    Get sqlite connection
    @type specs: Specs
    @rtype: sqlite3.Connection
    """
    conn = sqlite3.connect(specs.dbfile)
    conn.row_factory = sqlite3.Row
    def hastags(self, x, y):
        """Checks if y matches the tag pattern x,
            an example of a tag pattern is 'aaa,bbb|^ccc' """
        xs = x.split("|")
        xss = map(lambda u: u.split(","), xs)
        ys = y.split(",")
        def myMatch1S(pat, strs):
            if pat[0] == "^":
                return functools.reduce(
                    lambda p, q: ( p and q ),
                    map(lambda str_: pat[1:] != str_, strs),
                    True)
            else:
                return functools.reduce(
                    lambda p, q: ( p or q ),
                    map(lambda str_: pat == str_, strs),
                    False)
        def myMatchSS(pats, strs):
            return functools.reduce(
                lambda p, q: (p and q),
                map(lambda pat: myMatch1S(pat, strs), pats),
                True)
        answer = functools.reduce(
            lambda p, q: (p or q),
            map(lambda u: myMatchSS(u, ys), xss),
            False)
        if (answer):
            return 1
        else:
            return 0
    def eqsql(x, y):
        return x == y
    conn.create_function("tags", 2, hastags)
    conn.create_function("eqli", 2, eqsql)
    return conn

def sqlite_execute(specs, query, data):
    """
    Execute an sqlite statement and return the resulting list of results
    @type specs: Specs
    @type query: str
    @type data: tuple
    @rtype: list
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
    @type specs: Specs
    @type tbl: Tbl
    @type data: dict
    """
    qmarks = " (" + ",".join(list("?" * len(data.keys()))) + ") "
    query = "INSERT INTO " + tbl.name + " (" + ",".join(data.keys()) + ") " + " VALUES " + qmarks
    sqlite_execute(specs, query, tuple([data[k] for k in data.keys()]))

def sqlite_delete_values(specs, tbl, data):
    """
    Delete values from the table
    @type specs: Specs
    @type tbl: Tbl
    @type data: dict
    """
    query = "DELETE FROM " + tbl.name + " WHERE " + " AND ".join(["eqli(" + k + ",?)" for k in data.keys()])
    print(query)
    print(" ; ".join([data[k] for k in data.keys()]))
    sqlite_execute(specs, query, tuple([data[k] for k in data.keys()]))

def sqlite_update_values(
        specs: Specs,
        tbl: Tbl,
        old_data: dict,
        new_data: dict
):
    assignments = ", ".join([name + " = ? " for name in new_data.keys()])
    conditions = " AND ".join(["eqli(" + k + ",?)" for k in old_data.keys()])
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

class ColumnHighlightToggles:
    def __init__(self, tbl, state, grid):
        """
        Row of column labels (part of a Buttons onject)
        @param tbl: Tbl
        @param state: ButtonsState
        @param grid: Gtk.Grid
        """
        self.toggle = {}
        clmns_to_show = [ c for c in tbl.columns if c.name in state.columns_to_show ]
        self.topleft_label = Gtk.Label()
        grid.add(self.topleft_label)
        prev_toggle  = None
        for c in clmns_to_show :
            self.toggle[c.name] = Gtk.ToggleButton(label = c.name)
            if c.name in state.highlighted: self.toggle[c.name].set_active(True)
            if prev_toggle:
                grid.attach_next_to(self.toggle[c.name], prev_toggle, Gtk.PositionType.RIGHT, 1, 1)
            else:
                grid.attach_next_to(self.toggle[c.name], self.topleft_label, Gtk.PositionType.RIGHT, 1, 1)
            prev_toggle = self.toggle[c.name]

class Row:
    def __init__(self, specs, tbl, row_of_items, grid, prev, n):
        """
        Row of item labels (part of a Buttons object)
        @type specs: Specs
        @type tbl: Tbl
        @type row_of_items: sqlite3.Row
        @type grid: Gtk.Grid
        @type prev: Gtk.Widget
        @type n: int
        """
        self.specs = specs
        self.tbl = tbl
        self.row_of_items = row_of_items
        self.item_button = Gtk.Button("[F" + str(n) + "] " + row_of_items[0])
        self.item_button.connect("clicked", self.update_fn)
        self.labels = {}
        grid.attach_next_to(self.item_button, prev, Gtk.PositionType.BOTTOM, 1, 1)
        j = 0; prev_label = None
        for k in row_of_items.keys() :
            item = row_of_items[k]
            self.labels[k] = Gtk.Label()
            self.labels[k].set_name("ItemLabel" + str(j % 7))
            truncate_to_length = [clmn for clmn in tbl.columns if clmn.name == k][0].width
            self.labels[k].set_markup(
                xml.sax.saxutils.escape(truncate(item, truncate_to_length))
            )
            self.labels[k].set_tooltip_markup(xml.sax.saxutils.escape(truncate(item, Parameters.tooltip_truncate)))
            if prev_label:
                grid.attach_next_to(self.labels[k], prev_label, Gtk.PositionType.RIGHT, 1, 1)
            else:
                grid.attach_next_to(self.labels[k], self.item_button, Gtk.PositionType.RIGHT, 1, 1)
            prev_label = self.labels[k]
            j = j + 1
    def update_fn(self, button):
        p = Prefill(dict(zip(list(self.row_of_items.keys()), self.row_of_items)), ['UPDATE'])
        collector = CollectorGUI(self.specs, self.tbl, p)
        collector.initUI()

class Prefill:
    def __init__(self, data, flags):
        """
        Prefill object
        @param data: dict[str, str]
        @param flags: list
        """
        self.data = data
        self.flags = flags
        self.columns = data.keys()
        allowed_flags = ['UPDATE', 'DELETE', 'READONLY']
        for f in flags: assert f in allowed_flags
    def empty(columns, flags):
        empty_data = dict([(c,"") for c in columns])
        return Prefill(empty_data, flags)

class CollectorGUI(Gtk.Window):
    """
    Collector window
    @type specs: Specs
    @type table: Tbl
    @type prefill: Prefill
    """
    def __init__(self, specs, table, prefill):
        self.specs = specs
        self.table = table
        self.prefill = prefill
        self.label = {}
        self.text_entry = {}
        self.text_buffer = {}
        self.scrollwin = {}
        Gtk.Window.__init__(self)
    def on_key_press_event(self, widget, event):
        keyname = Gdk.keyval_name(event.keyval)
        state = event.state
        #print("Key %s (%d) was pressed" , (keyname, event.keyval))
        for j in range(1,13):
            if keyname == "F" + str(j):
                ctrl = 12 if ( state &  Gdk.ModifierType.CONTROL_MASK ) else 0
                self.text_entry[self.table.columns[j-1 + ctrl]].grab_focus()
    def on_key_release_event(self, widget, event): pass
    def wrap(self, widget, name):
        """
        prepare a customized widget
        @type widget:
        @type name:
        @rtype: Gtk.Widget
        """
        widget.set_name(name)
        widget.connect("key_press_event", self.on_key_press_event)
        widget.connect("key_release_event", self.on_key_release_event)
        return widget
    def initUI(self):
        vbox = self.wrap(Gtk.VBox(), name = "CollectorBox")
        self.add(vbox)
        tophbox = Gtk.HBox()
        bottomhbox = Gtk.HBox()
        grid = Gtk.Grid()
        grid.set_row_spacing(3)
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
    def get_data_dict(self):
        data_dict = {}
        for column in self.table.columns:
            if column in self.text_buffer.keys():
                buffer = self.text_buffer[column]
                data_dict[column.name] = buffer.get_text(buffer.get_start_iter(), buffer.get_end_iter(), True)
        return data_dict
    def unlock_fn(self, button):
        self.prefill.flags = ['UPDATE']
        new_collector = CollectorGUI(self.specs, self.table, self.prefill)
        self.destroy()
        new_collector.initUI()
    def confirm_delete_fn(self, button):
        self.prefill.flags = ['DELETE']
        new_collector = CollectorGUI(self.specs, self.table, self.prefill)
        self.destroy()
        new_collector.initUI()
    def yes_dodelete_fn(self, button):
        data_dict = self.get_data_dict()
        sqlite_delete_values(self.specs, self.table, data_dict)
        self.destroy()
    def no_dontdelete_fn(self, button):
        self.prefill.flags = ['UPDATE', 'READONLY']
        new_collector = CollectorGUI(self.specs, self.table, self.prefill)
        self.destroy()
        new_collector.initUI()
    def collect_fn(self, button):
        data_dict = self.get_data_dict()
        sqlite_insert_values(self.specs, self.table, data_dict)
        self.destroy()
    def update_fn(self, button):
        data_dict = self.get_data_dict()
        print(self.prefill.data[self.table.primary_key])
        print(data_dict[self.table.primary_key])
        if self.prefill.data[self.table.primary_key] == data_dict[self.table.primary_key]:
            sqlite_update_values(self.specs, self.table, self.prefill.data, data_dict)
        else:
            sqlite_insert_values(self.specs, self.table, data_dict)
        self.destroy()

class ButtonsState:
    """
    Information about the state of the Buttons widget
    @type columns_to_show: list[str]
    @type rows: list[Row]
    @type highlighted: set[str]
    """
    def __init__(self, columns_to_show, rows, highlighted):
        self.columns_to_show = columns_to_show
        self.rows = rows
        self.highlighted = highlighted

class Buttons(Gtk.VBox):
    """
    Buttons
    @type specs: Specs
    @type table: Tbl
    @type state: ButtonsState
    """
    def __init__(self, specs, table, state):
        self.specs = specs
        self.table = table
        self.state = state
        self.tophbox = Gtk.HBox()
        self.grid = Gtk.Grid()
        Gtk.VBox.__init__(self)
        self.set_name("ButtonsVBox")
        self.add(self.tophbox)
        self.add(self.grid)
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
        prev = self.column_highlight_toggles.topleft_label
        for row in self.state.rows:
            r = Row(self.specs, self.table, row, self.grid, prev, 1 + len(self.displayed_rows))
            for k in r.labels.keys():
                if k in self.state.highlighted:
                    r.labels[k].set_name("HighlightedLabel")
            self.displayed_rows.append(r)
            prev = r.item_button
        i = 0
        for clmn in self.state.columns_to_show:
            self.column_highlight_toggles.toggle[clmn].connect("toggled", highlight_pre_fn(clmn))
            i = i + 1


class Context():
    """
    For the widget to know about it parents etc
    @type commander: Commander
    @type escape_to: Gtk.Widget
    @type mainwin: Gtk.Window
    """
    def __init__(self, commander, escape_to, mainwin):
        self.commander = commander
        self.escape_to = escape_to
        self.mainwin = mainwin

class Results(Gtk.VBox):
    """
    Show results
    @type specs: Specs
    @type table: Tbl
    @type columns_to_show: list[str]
    @type query: str
    @type data: tuple
    """
    def __init__(self, specs, table, context, columns_to_show, query, data):
        self.specs = specs
        self.table = table
        self.context = context
        self.columns_to_show = columns_to_show
        self.query = query
        self.data = data
        self.buttons = None
        self.batch_no = 0
        Gtk.VBox.__init__(self)
        self.connect("key_press_event", self.on_key_press_event)
        self.set_name("Results")
        self.set_can_focus(True)
        self.initWidget()
    def initWidget(self):
        rows = sqlite_execute(self.specs, self.query, self.data)
        self.tophbox = Gtk.HBox()
        self.go_prev_batch_button = Gtk.Button("<-" if self.batch_no > 0 else "--")
        exist_more_batches = self.batch_no < int((len(rows) - 1)/Parameters.results_batch_size)
        self.go_next_batch_button = Gtk.Button(
            "->" if exist_more_batches else "--"
        )
        if self.batch_no > 0: self.go_prev_batch_button.connect("clicked", self.go_prev_batch_fn)
        if exist_more_batches: self.go_next_batch_button.connect("clicked", self.go_next_batch_fn)
        self.tophbox.add(self.go_prev_batch_button)
        self.tophbox.add(self.go_next_batch_button)
        self.refresh_btn = Gtk.Button("refresh")
        self.refresh_btn.connect("clicked", self.refresh_fn)
        self.tophbox.add(self.refresh_btn)
        self.new_btn = Gtk.Button("new")
        self.new_btn.connect("clicked", self.new_fn)
        self.tophbox.add(self.new_btn)
        self.destroy_btn = Gtk.Button("destroy")
        self.destroy_btn.connect("clicked", self.destroy_fn)
        self.tophbox.add(self.destroy_btn)
        batch_size = Parameters.results_batch_size
        self.buttons = Buttons(
            self.specs,
            self.table,
            ButtonsState(
                self.columns_to_show,
                rows[ self.batch_no * batch_size: (self.batch_no + 1) * batch_size ],
                self.buttons.state.highlighted if self.buttons else set()
            )
        )
        self.bottomhbox = Gtk.HBox()
        self.add(self.tophbox)
        self.add(self.buttons)
        self.add(self.bottomhbox)
        self.show_all()
        self.context.mainwin.resize(1,1)
    def on_key_press_event(self, widget, event):
        keyname = Gdk.keyval_name(event.keyval)
        state = event.state
        print("Key %s (%d) was pressed in %s" , (keyname, event.keyval, widget.get_name()))
        for j in range(1,13):
            if keyname == "F" + str(j):
                r = self.buttons.displayed_rows[j-1]
                r.update_fn(r.item_button)
    def new_fn(self,b):
        prefill = Prefill.empty([c.name for c in self.table.columns if c.do_show], [])
        collector = CollectorGUI(self.specs, self.table, prefill)
        collector.initUI()
    def refresh_fn(self, b):
        self.tophbox.destroy()
        self.buttons.destroy()
        self.bottomhbox.destroy()
        self.initWidget()
    def go_prev_batch_fn(self, b):
        self.batch_no = self.batch_no - 1
        self.refresh_fn(b)
    def go_next_batch_fn(self, b):
        self.batch_no = self.batch_no + 1
        self.refresh_fn(b)
    def destroy_fn(self, b):
        self.destroy()
        self.context.commander.results.remove(self)
        self.context.mainwin.resize(1,1)

class Commander(Gtk.VBox):
    """
    Topmost widget: the commandline
    @type specs: Specs
    @type table: Tbl
    @type mainwin: Gtk.Window
    """
    def __init__(self, specs, table, mainwin):
        self.specs = specs
        self.table = table
        self.mainwin = mainwin
        Gtk.VBox.__init__(self)
        self.set_name("CommanderTopVBox")
        self.results = []
    def on_key_press_event(self, widget, event):
        keyname = Gdk.keyval_name(event.keyval)
        state = event.state
        print("Key %s (%d) was pressed in %s" , (keyname, event.keyval, widget.get_name()))
        if keyname == "colon":
            self.cmdline.grab_focus()
        if widget.get_name() == "CommanderTop":
            for j in range(1,13):
                if keyname == "F" + str(j):
                    self.toggle[j-1].set_active(not(self.toggle[j-1].get_active()))
            else:
                for j in range(1, len(self.results) + 1):
                    if keyname == str(j): self.results[j-1].grab_focus()
        if keyname == "Escape":
            self.top.grab_focus()
        if state &  Gdk.ModifierType.CONTROL_MASK:
            if keyname == "Return" : self.rows_fn(self)
    def on_key_release_event(self, widget, event):
        keyname = Gdk.keyval_name(event.keyval)
    def wrap(self, widget, name):
        """
        prepare a customized widget
        @type widget:
        @type name:
        @rtype: Gtk.Widget
        """
        widget.set_name(name)
        widget.connect("key_press_event", self.on_key_press_event)
        widget.connect("key_release_event", self.on_key_release_event)
        return widget
    def initWidget(self):
        self.top = self.wrap(Gtk.VBox(), "CommanderTop")
        self.top.set_can_focus(True)
        self.cmdline_box = self.wrap(Gtk.VBox(), "CommanderCmdlineBox")
        self.toggles_box = self.wrap(Gtk.HBox(), "CommanderTogglesBox")
        self.bottom = self.wrap(Gtk.VBox(), "CommanderBottom")
        self.add(self.top)
        self.add(self.bottom)
        self.top.add(self.cmdline_box)
        self.top.add(self.toggles_box)
        self.toggle = []
        self.primary_key_label = self.wrap(Gtk.Label(), "PrimaryKeyLabel")
        self.primary_key_label.set_markup(xml.sax.saxutils.escape(self.table.primary_key))
        self.toggles_box.add(self.primary_key_label)
        for j in range(1,len(self.table.columns)):
            self.toggle.append(
                self.wrap(Gtk.ToggleButton(label = self.table.columns[j].name), "CommanderToggleButton")
            )
            self.toggles_box.add(self.toggle[j-1])
        self.cmdline = self.wrap(Gtk.TextView(), "CommandLine")
        self.cmd_buffer = self.cmdline.get_buffer()
        self.rows_button = self.wrap(Gtk.Button("rows"), "RowsButton")
        self.rows_button.connect("clicked", self.rows_fn)
        self.cmdline_box.add(self.cmdline)
        self.cmdline_box.add(self.rows_button)
        self.show_all()
    def rows_fn(self,b = None):
        columns_selected = [
            self.table.columns[j].name
            for j in range(1,len(self.table.columns)) if self.toggle[j-1].get_active()
        ]
        what_to_select = ",".join([self.table.primary_key] + columns_selected) if columns_selected else "*"
        columns_to_show = ([self.table.primary_key] + columns_selected) if columns_selected else [c.name for c in self.table.columns]
        text_in_cmd_buffer = self.cmd_buffer.get_text(
            self.cmd_buffer.get_start_iter(),
            self.cmd_buffer.get_end_iter(),
            True)
        if text_in_cmd_buffer:
            query = "SELECT " + what_to_select + " FROM " + self.table.name + " WHERE " + text_in_cmd_buffer
        else:
            query = "SELECT " + what_to_select + " FROM " + self.table.name
        print(query)
        r = Results(self.specs, self.table, Context(self, self, self.mainwin), columns_to_show, query, None)
        self.bottom.add(r)
        self.results.append(r)

def table_chooser(specs):
    """
    Choose a table from the list
    @type specs: Specs
    @return: Tbl
    """
    win = Gtk.Window(name = "TableChooserMainwin")
    win.connect("delete-event", Gtk.main_quit) # this is to abort the execution of the program
    register_css(specs.css)
    vbox = Gtk.VBox(name = "TableChooserVBox")
    win.add(vbox)
    l = {}
    hinted_tables = dict(zip(Parameters.alphabet, specs.tables))
    choosen_table = "a"
    for p in zip(Parameters.alphabet, specs.tables):
        k = p[0]
        l[k] = Gtk.Label()
        l[k].set_markup("<b>" + k + "</b>: " + hinted_tables[k].name )
        vbox.add(l[k])
    def on_key_press_event(widget, event):
        keyname = Gdk.keyval_name(event.keyval)
        state = event.state
        print("Key %s (%d) was pressed in %s" , (keyname, event.keyval, widget))
        nonlocal choosen_table
        for k in hinted_tables.keys():
            if keyname == k:
                choosen_table = hinted_tables[k]
        win.destroy()
        Gtk.main_quit()
    win.connect("key_press_event", on_key_press_event)
    win.show_all()
    Gtk.main()
    return choosen_table


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
    win = Gtk.Window(name = "MainWindow")
    win.connect("delete-event", Gtk.main_quit)
    register_css(mytable.css)
    cmdr = Commander(myspecs, mytable, win)
    win.add(cmdr)
    cmdr.initWidget()
    win.show_all()
    Gtk.main()
