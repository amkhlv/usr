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

### Discovering the structure of the document

    xmlstarlet el "example.xml"

or, more concisely:

    xmlstarlet el -u "example.xml"


### XPath specifications

    echo '<a><a1>b</a1></a>' | xmlstarlet sel -t -v "/a/a1"

or:

    echo '<a><a1>b</a1></a>' | xmlstarlet sel -t -v "//a1"

--- both give `b`. (Leading `//` means search all the way down.)

Select value with given attribute value (in this case the attribute is called "key"):

    xmlstarlet sel -t -v '//*[@key="upload-url"]' bystrotex.xml

General rules:

    nodename	Selects a node and all of its children
    /	        Selects the document root
    //	        Selects a node and all of its children; regardless of where it is located in the XML document hierarchy.
    @	        Selects an attribute

### Subtlety with namespaces

This is __important__

If you have a document with namespaces, you need to specify them in the XPath expression. For example:

    xmlstarlet sel -N s=http://www.w3.org/2000/svg  -t -v '//s:desc'   myfile.svg

Without the namespace specification, the command will not work.
