#!/usr/bin/python

import os
import yaml
import curses
import subprocess

YAML_FILENAME = os.environ['HOME']+"/.config/amkhlv/tasks.yaml"
chars="abcdefghijklmnopqrstuvwxyz"

if __name__ == '__main__':
    with open(YAML_FILENAME, 'r') as yamlfl:
        y = yaml.safe_load(yamlfl)
    c = dict(zip(list(chars), y.keys()))

    stdscr = curses.initscr()
    curses.noecho()
    curses.cbreak()
    j=0
    for k in c.keys():
        stdscr.addstr(j, 0, k + ": " + c[k])
        j = j + 1
    key = stdscr.getch()
    curses.nocbreak(); stdscr.keypad(0); curses.echo()
    curses.endwin()

    print(c[chr(key)])
    print
    os.system(y[c[chr(key)]])
    

