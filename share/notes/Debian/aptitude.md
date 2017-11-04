Search
======

Search terms
------------

    aptitude search "?provides(ssh)"

The table of search terms [is here](https://aptitude.alioth.debian.org/doc/en/ch02s04s05.html#tableSearchTermQuickGuide).
For example:

    aptitude search "?installed ?provides(ssh)"

is equivalent to:

    aptitude search "~i?provides(ssh)"

and to:

    aptitude search "~i~Pssh"

Display format
--------------

[Is explained here](https://www.debian.org/doc/manuals/aptitude/ch02s05s01.en.html#secDisplayFormat)

Sorting
-------

Sort all installed packages by size:

    aptitude search "~i" --sort installsize

To see install sizes and descriptions, use `Display format` as above:

    aptitude search "~i" --sort installsize -F "%I %p %d"

