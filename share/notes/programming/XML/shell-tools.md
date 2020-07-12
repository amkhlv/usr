# Shell tools for processing XML

## xmllint

this can pretty-print, and search for a value in an XML file

### Installation

    aptitude install libxml2-utils

### Use

    xmllint --xpath 'string(//email)' student.xml

--- selects `<email>...</email>`

    wget http://www.example.com -O- | xmllint --html --format -

--- pretty prints HTML 


## xmlstarlet

This is a very powerful processor

### Installation

    aptitude install xmlstarlet

### Use examples

    echo '>' | xmlstarlet esc 

gives `&gt;` --- this very handy in Emacs, when editing XML !

    echo '<a>b</a>' | xmlstarlet sel -t -v "/a"

gives `b`

    cat bills.xml | xmlstarlet sel -t --match '//bill' --if "@payee='PortoSeguro'" --value-of 'amount' --nl

--- here `--nl` prints newline after each match

