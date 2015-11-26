#!/usr/bin/env python3

import http.client, urllib
import argparse
import os
import sqlite3

DBASE = os.environ['HOME'] + "/.config/amkhlv/misc.sqlite"
SPA_URL="/bsipura.spa"
SPEEDDIAL2_KEY="28078"

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
    params = urllib.parse.urlencode({SPEEDDIAL2_KEY : number})
    headers = {"Content-type": "application/x-www-form-urlencoded", "Accept": "text/plain"}
    conn.request("POST", SPA_URL, params, headers)
    response = conn.getresponse()
    print(response.status)

def get_address_and_port():
    sqlite_conn = sqlite3.connect(DBASE)
    cursor = sqlite_conn.cursor()
    cursor.execute("SELECT ip,port FROM ips WHERE nick=?",("linksys",))
    (x,y) = cursor.fetchone()
    sqlite_conn.close()
    return x,y

if __name__ == "__main__":
    command_line_arguments(parser)
    options = parser.parse_args()
    spa_address, spa_port = get_address_and_port()
    conn = http.client.HTTPConnection(spa_address, spa_port)
    set_speeddial2(conn, "".join([x for x in list(options.number) if x not in [" ", ".", "-", ")", "("]]))
    conn.close()
