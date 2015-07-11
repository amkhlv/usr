LaTeX counters
==============

Setup a counter
---------------
    \newcounter{Conjectures}
    \setcounter{Conjectures}{0}
    ...


Increment a counter
-------------------

    \refstepcounter{Conjectures}
    \noindent{\bf Conjecture \arabic{Conjectures}\label{ConjectureAboutX}: 


Refer to a counter
------------------

    If $X$ satisfies Conjecture \ref{ConjectureAboutX} ...

