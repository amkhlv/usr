#!/usr/bin/env python

import httplib, urllib
import argparse
import os
import sqlite3

DBASE = os.environ['HOME'] + "/.config/amkhlv/misc.sqlite"
SPA_URL="/bsipura.spa"

instructions="""
Linksys SPA interface
=====================
With the handset on-hook, execute this command to set the phone number you wish to dial.
Then, with the handset still on-hook, press 2 on the telephone keypad. 
Then lift the handset. The telephone should start dialing automatically.
"""

parser = argparse.ArgumentParser(description=instructions, formatter_class=argparse.RawTextHelpFormatter)

def command_line_arguments(parser):
    """Invocation"""
    parser.add_argument("args", nargs = '*')
    parser.add_argument("-n", "--number",
                        help="number to set")

def set_speeddial2(conn, number):
    params = urllib.urlencode({'28078': number})
    headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
    conn.request("POST", SPA_URL, params, headers)
    response = conn.getresponse()
    print response.status, response.reason
    data = response.read()
    data

if __name__ == "__main__":
    sqlite_conn = sqlite3.connect(DBASE)
    cursor = sqlite_conn.cursor()
    cursor.execute("SELECT ip,port FROM ips WHERE nick=?",("linksys",))
    x,y = cursor.fetchone()
    spa_address = x.encode('ascii', 'ignore')
    spa_port = y.encode('ascii', 'ignore')
    sqlite_conn.close()
    command_line_arguments(parser)
    options = parser.parse_args()
    conn = httplib.HTTPConnection(spa_address, spa_port)
    set_speeddial2(conn, "".join([x for x in list(options.number) if x not in [" ", ".", "-", ")", "("]]))
    conn.close()
