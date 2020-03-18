# sed

Excellent introduction is [found here](https://www.grymoire.com/Unix/Sed.html)

## In-place

    sed -i.bak 's/red/blue/g' myfile.txt

## Insert before line, append after line, change line

    #!/bin/sh
    sed '
    /WORD/ i\
    Add this line before every line with WORD
    '

    #!/bin/sh
    sed '
    /WORD/ a\
    Add this line after every line with WORD
    '

To change the current line with a new line:

    #!/bin/sh
    sed '
    /WORD/ c\
    Replace the current line with the line
    '

(Note skillfull use of '\' before newline.)

## Multiline patterns. Commands N,D,P,H and n,d,p,h

