curses
======

Contents:

.. toctree::
   :maxdepth: 2

To initiate::

    import curses
    stdscr = curses.initscr()

Usually curses applications turn off automatic echoing of keys to the screen, 
in order to be able to read keys and only display them under  certain circumstances. 
This requires calling the noecho() function::

    curses.noecho()

Applications will also commonly need to react to keys instantly, without requiring 
the Enter key to be pressed; this is called cbreak mode, as opposed to the usual buffered input mode::

    curses.cbreak()

Terminals usually return special keys, such as the cursor keys or navigation keys such as Page Up and Home, as a multibyte escape sequence. While you could write your application to expect such sequences and process them accordingly, curses can do it for you, returning a special value such as curses.KEY_LEFT. To get curses to do the job, you’ll have to enable keypad mode::

    stdscr.keypad(1)

To add string at the given position::

    stdscr.addstr(vert, hor, "hello")

To read keypress::

    key = stdscr.getch()
    print(chr(key))

Terminating a curses application is much easier than starting one. You’ll need to call::

    curses.nocbreak(); stdscr.keypad(0); curses.echo()
    curses.endwin()



