# Useful links

[Locally saved book of Knuth](file:///home/andrei/a/books/computer/Donald_Knuth_-_The_Tex_Book.pdf)

__A very nice__ [cheatsheet](file://texcrib.pdf) on TeX including boxes.

# Boxes

    \newcommand{\mystackrel}[2]{%
      \setbox0 = \vbox{\hbox{$#2$}} %
      \stackrel{#1}{#2}\kern -3pt %
      \vbox to \ht0 {%
        \hbox to 1pt{} %
      } %
    }
    \newcommand{\nablaflat}{%
      \setbox0 = \vbox{\hbox{$\nabla$}} %
      \stackrel{\rm\tiny flat}{\nabla}\kern -5pt %
      \vbox to \ht0 {%
        \hbox to 1pt{} %
      } %
    }
