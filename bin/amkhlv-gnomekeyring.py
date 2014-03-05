#!/usr/bin/env python2

import pygtk
import gnomekeyring
import sys
import yaml

fh = open('~/.config/amkhlv/gnome-keyring.yaml')
y = yaml.safe_load(fh)
fh.close()

gnomekeyring.unlock_sync(y['login'],y['password'])


sys.exit(0)

