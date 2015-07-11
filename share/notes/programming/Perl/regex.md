Syntax of Regex
===============

Metacharacters
--------------

     \ Quote the next metacharacter
    ^ Match the beginning of the line
    . Match any character (except newline)
    $ Match the end of the string (or before newline at the end
    of the string)
    | Alternation
    () Grouping
    [] Bracketed Character class


Quantifiers
-----------

    * Match 0 or more times
    + Match 1 or more times
    ? Match 1 or 0 times
    {n} Match exactly n times
    {n,} Match at least n times
    {n,m} Match at least n but not more than m times


Escape sequences
----------------

    \t tab (HT, TAB)
    \n newline (LF, NL)
    \r return (CR)
    \f form feed (FF)
    \a alarm (bell) (BEL)
    \e escape (think troff) (ESC)
    \cK control char (example: VT)


Character Classes and other Special Escapes
-------------------------------------------

    \W Match a non-"word" character
    \s Match a whitespace character
    \S Match a non-whitespace character
    \d Match a decimal digit character
    \D Match a non-digit character


Assertions
----------

    \b{} Match at Unicode boundary of specified type
    \B{} Match where corresponding \b{} doesn't match
    \b Match a word boundary
    \B Match except at a word boundary
    \A Match only at beginning of string
    \Z Match only at end of string, or before newline at the end
    \z Match only at end of string
    \G Match only at pos() (e.g. at the end-of-match position
    of prior m//g)


Use of regex
============

Capture
-------

The bracketing construct `( ... )` creates capture groups (also referred to as capture buffers).
To refer to the current contents of a group later on, within the same pattern,
use `\g1` (or `\g{1}` ) for the first, `\g2` (or `\g{2}` ) for the second, and so on.


Extracting matches
------------------

The grouping metacharacters `()` also allow the extraction of the parts of a string that matched.
For each grouping, the part that matched inside goes into the special variables `$1` , `$2` , etc. 
