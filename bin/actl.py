#!/usr/bin/python3

import yaml
import os
from os.path import expanduser
import json
import subprocess
from time import sleep
import threading
import signal
from typing import Dict, Union, List

stream = open(expanduser("~/.config/amkhlv/actl.yaml"),'r')
progs = yaml.load(stream)


def init_procs() -> Dict[str, Union[None, subprocess.Popen]]:
    return dict([(name, None) for name in progs.keys()])


procs = init_procs()


def check_if_running(p: subprocess.Popen):
    if p.poll() is None: return 1
    else: return 0


def state():
    x = {"xs":
            [{"name": xn, "isActive": check_if_running(procs[xn]) if procs[xn] else 0}
             for xn in progs.keys()]
         }
    return json.dumps(x) + "\n"


ioqml = subprocess.Popen(
    ["ioqml", expanduser("~/usr/lib/QML/actl.qml")],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE
)


def toggle_proc(name):
    p = procs[name]
    if p and p.poll() is None:
        print("-- killing " + name)
        p.kill()
    else:
        print("-- respawing: " + name)
        procs[name] = subprocess.Popen(
            [str(a) for a in progs[name]['args']],
            env=progs[name].get('env',None)
        )


def flush_state():
    if ioqml.stdin.closed: pass
    else:
        ioqml.stdin.write(state().encode('utf-8'))
        ioqml.stdin.flush()


class Watcher(threading.Thread):
    def run(self):
        self.alive = True
        while self.alive:
            flush_state()
            sleep(1)
        ioqml.stdin.close()
        ioqml.stdout.close()
    def stop(self):
        self.alive = False


flush_state()
t = Watcher()
t.start()
print(progs)
print(state())
for line in iter(ioqml.stdout.readline, b''):
    print(line.rstrip())
    toggle_proc(line.rstrip().decode('utf-8'))
print("-- exiting")
t.stop()
