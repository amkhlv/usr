#!/usr/bin/python3

from os.path import expanduser
import json
import subprocess


if __name__ == "__main__":
    ioqml = subprocess.Popen(
        ["ioqml", expanduser("~/usr/lib/QML/oneline-entry.qml")],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE
    )
    ioqml.stdin.write(
        (json.dumps(
            {"w": 1000 , "winttl": "XDG open" , "lblfontsize": 24, "entryfontsize": 20, "prompt": "enter URI"}
        ) + "\n").encode('utf-8')
    )
    ioqml.stdin.flush()
    for line in iter(ioqml.stdout.readline, b''):
        subprocess.Popen(["xdg-open", line])
    ioqml.stdout.close()
    ioqml.stdin.close()

