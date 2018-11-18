LaTeX counters
==============

Setup a counter
---------------
    \newcounter{Conjectures}
    \setcounter{Conjectures}{0}
    ...


Increment a counter
-------------------

    {\refstepcounter{Conjectures}\label{ConjectureAboutX}}
    \noindent{\bf Conjecture \arabic{Conjectures}: }


Refer to a counter
------------------

    If $X$ satisfies Conjecture \ref{ConjectureAboutX} ...

