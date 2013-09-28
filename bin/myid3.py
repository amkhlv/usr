#!/usr/bin/python

__author__    = "Andrei Mikhailov"
__copyright__ = "Copyright 2013, Andrei Mikhailov"
__license__   = "GPL"

import sys
import mutagen.id3
import re
from optparse import OptionParser

notug = "---"

#################################################################################

parser = OptionParser()
parser.add_option("-0", dest="null_sep", default=False, action="store_true",
                  help="""produces null-separated output""")
parser.add_option("--all", dest="all_data", default=False, action="store_true",
                  help="""outputs all information""")
(options, args) = parser.parse_args()

filelist =  map(lambda x: x.rstrip('\n') , sys.stdin.readlines()) 

def get_track (filename):
    try:
        x = mutagen.id3.Open(filename)
        return re.sub(r'\/.*'  ,  ''  ,   x['TRCK'].text[0] )
    except:
        sys.stderr.write("NOTUG: " + filename +'\n')
        return notug

names_with_tags = map(lambda x: (get_track(x), x) , filelist)
valid_files     = filter(lambda x: x[0] != notug ,  names_with_tags)

if options.null_sep :
    separator = "\0"
else :
    separator = "\n"

output = ""
for x in sorted(valid_files, key = lambda fn : int(fn[0])):
    if options.all_data :
        y = mutagen.id3.Open(x[1])
        for z in y.keys():
            print str(z) + "[" + x[1] + "]="+ str(y[z])
    else :
        if (output == "") :
            output = output + x[1]
        else :
            output = output + separator + x[1]

sys.stdout.write(output)
