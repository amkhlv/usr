#!/bin/bash

[ -e "/home/andrei/.Xmodmap" ] && xmodmap /home/andrei/.Xmodmap
BASH_ENV=/home/andrei/.amkhlv bash -c gmrun
