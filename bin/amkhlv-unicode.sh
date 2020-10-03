#!/usr/bin/env bash

export IGNORECASE=1
gawk\
  'BEGIN { 
    n = 20 ;
    IGNORECASE = 1;
  }
  ( n > 0 ) && (match($0,"'$(echo $@ | sed -e's/\s/.*/g')'")) { 
      gsub("&#x","") ;
      gsub(";","") ;
      system("notify-send -u normal -t 5000 \""$0"\"") ;
      n = n - 1;
    }'\
  ~/usr/share/notes/unicode.txt

