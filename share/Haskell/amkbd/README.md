# amkbd

This is to auto-type some text.

The list of symbols `keysyms.txt` is taken [from here](http://wiki.linuxquestions.org/wiki/List_of_Keysyms_Recognised_by_Xmodmap) but with added Cyrillic ё 

It __should be copied to__ `/usr/local/lib/amkhlv/`

The script `autokeys.sh` works like this:

    echo what I want to type | autokeys.sh

Then, to trigger typing:

    echo go > ~/.amkhlv-keyboardpipe.fifo

To clean the pipe use `keyboard-cleaner.sh`
