Including hyperlinks
====================

Header Required
---------------

    \usepackage{hyperref}

External link
-------------

    \href{http:www.google.com}{Google}

Link to section in text
-----------------------

In file `animals.tex` put a ``label'' like this:

    \hypertarget{CommentOnPropertiesOfDogs}{}

Then, when you want to cite it (perhaps in another file):

    \href{animals.pdf#CommentOnPropertiesOfDogs}{See notes about dogs}

