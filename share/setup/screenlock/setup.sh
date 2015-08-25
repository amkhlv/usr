#!/bin/bash

raco exe amkhlv-screensaver.rkt

mv amkhlv-screensaver /usr/local/bin/

chown root:root /usr/local/bin/amkhlv-screensaver

chmod go-x /usr/local/bin/amkhlv-screensaver
chmod go-r /usr/local/bin/amkhlv-screensaver



