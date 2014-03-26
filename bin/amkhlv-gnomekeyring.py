#!/usr/bin/env python2

import pygtk
import gnomekeyring
import sys
import os
import yaml

fh = open(os.environ['HOME'] +'/.config/amkhlv/gnome-keyring.yaml')
y = yaml.safe_load(fh)
fh.close()

gnomekeyring.unlock_sync(None,y['password'])


sys.exit(0)

