Backslashitis
=============

When entering in the minibuffer, backslash is backslash: `\(...\)` , but when entering in `ELisp`, need `\\(...\\)`

Grouping
========

    \(...\)

and then:

    \1

Cheatsheet
==========

    .        any character (but newline)
    *        previous character or group, repeated 0 or more time
    +        previous character or group, repeated 1 or more time
    ?        previous character or group, repeated 0 or 1 time  
    ^        start of line
    $        end of line
    [...]    any character between brackets
    [^..]    any character not in the brackets
    [a-z]    any character between a and z
    \        prevents interpretation of following special char
    \|       or
    \w       word constituent
    \b       word boundary
    \sc      character with c syntax (e.g. \s- for whitespace char)
    \( \)    start\end of group
    \< \>    start\end of word
    \_< \_>  start\end of symbol
    \` \'    start\end of buffer\string
    \1       string matched by the first group
    \n       string matched by the nth group
    \{3\}    previous character or group, repeated 3 times
    \{3,\}   previous character or group, repeated 3 or more times
    \{3,6\}  previous character or group, repeated 3 to 6 times
    \=       match succeeds if it is located at point

Syntax classes:

    \s-   whitespace character        \s/   character quote character
    \sw   word constituent            \s$   paired delimiter
    \s_   symbol constituent          \s'   expression prefix
    \s.   punctuation character       \s<   comment starter
    \s(   open delimiter character    \s>   comment ender
    \s)   close delimiter character   \s!   generic comment delimiter
    \s"   string quote character      \s|   generic string delimiter 
    \s\   escape character

Characters are organized by category. Use `C-uC-x=` to display the category of the character under the cursor.

    \ca      ascii character
    \Ca      non-ascii character (newline included)
    \cl      latin character
    \cg      greek character

Here are some syntax classes that can be used between brackets, e.g. [[:upper:]\|[:digit:]\.].

    [:digit:]  a digit, same as [0-9]
    [:alpha:]  a letter (an alphabetic character)
    [:alnum:]  a letter or adigit (an alphanumeric character ()
    [:upper:]  a letter in uppercase
    [:space:]  a whitespace character, as defined by the syntax table
    [:xdigit:] an hexadecimal digit
    [:cntrl:]  a control character
    [:ascii:]  an ascii character

