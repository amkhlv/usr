-*- coding: utf-8 -*-

# Babel

## Installing portuguese

    tlmgr init-usertree
    tlmgr install babel-portuges

## Latex headers

    \documentclass[12pt]{article} 
    \usepackage{latexsym}
    \usepackage[brazil]{babel}
    \usepackage[latin1]{inputenc}


# LuaLaTex with fontspec

## Shapka

In the shapka:

    \documentclass[12pt]{article}

    \usepackage{xcolor}
    \usepackage{fontspec}
    \usepackage[utf8]{inputenc}
    \usepackage[english,russian]{babel}

    \setmainfont{FreeSerif}

In Debian, the package `texlive-lang-cyrillic` should be installed.

__Important note about Emacs:__

When `emacs` sees `\usepackage[latin1]...` he thinks that the file
should be encoded in `latin1`. This is wrong, file should be encoded
in `utf-8-unix`, because this is standard on Debian, and it should be.
Therefore, we need to add `coding: utf-8-unix` in the first line
of the file between `-*-`

Explanations:

* fontspec

See Section ["About fontspec"](#about-fontspec) below

* setmainfont

Another __nice choice__ of the mainfont:

    \setmainfont[Scale=0.9]{DejaVuSerif}

* inputenc

We use the command: `\usepackage[latin1]{inputenc}`. One would think that
it is better to use `utf8` instead of `latin1`, but this is not the
case: \`\`The supported encoding by the LaTeX team is utf8 and covers a fairly
specific/limited range of unicode input characters. It only defines those
symbols that are known to be available with the current font
encoding. utf8x is not officially supported, but covers a much broader
range of input symbols.''

## About fontspec

### What is `fontspec`?

The fontspec package allows users of either `XeTeX` or `LuaTeX` to load [OpenType](http://en.wikipedia.org/wiki/OpenType)
fonts in a `LaTeX` document. No font installation is necessary, and font features can 
be selected and used as desired throughout the document.

### General font selection

       \fontspec [ font features ] { font name }
       \setmainfont [ font features ] { font name }
       \setsansfont [ font features ] { font name }
       \setmonofont [ font features ] { font name }
       \newfontfamily cmd [ font features ] { font name }

Fonts can be selected either by \`font name' or by \`file name'.

Fonts known to LuaTEX or XETEX may be loaded by their names. \`Known to' in this
case generally means \`exists in a standard fonts location'. The simplest example might be something like:

    \fontspec[ ... ]{FreeSerif}

in which the bold and italic fonts will be found automatically (if they exist).

So, I can change font on the fly:

    {\fontspec[Scale=2]{DejaVuSans} Hi there!} and back

I can also change color:

    \fontspec[Color=red]{DejaVuSans} examplered

but this requires `\usepackage{xcolor}` in the headers. 

### Example

       \setmainfont{Deja Vu Serif}
       \begin{document} 
       Пример of {\fontspec[Scale=2]{Penguin Attack} equação:} $x$
       \end{document}

# Usual LaTeX with fontenc

Speaking about just Cyrillic and Portuguese, it should not be necessary to
use `LuaLaTeX` and `fontspec`. Indeed, just a normal `LaTeX` should have
enough support for both Cyrillic and Portuguese.

__I have not tried it__, but perhaps the following will also work:

Support for Cyrillic is based on standard LATEX mechanisms plus the
fontenc and inputenc packages. But, __if you are going to use Cyrillics in math mode__, 
you need to load mathtext package before fontenc:

    \usepackage{mathtext}
    \usepackage[T1,T2A]{fontenc}
    \usepackage[koi8-ru]{inputenc}
    \usepackage[english,russian]{babel}

Generally, babel will authomatically choose the default font encoding, for
the above three languages this is T2A. However, documents are not restricted
to a single font encoding. For multi-lingual documents using Cyrillic and
Latin-based languages it makes sense to include Latin font encoding explicitly.
babel will take care of switching to the appropriate font encoding when a
different language is selected within the document.

# Japanese

    sudo apt-get install latex-cjk-japanese
    
    % (set-input-method 'japanese)
    \usepackage{CJK}
    % Local Variables:
    % coding: euc-jp
    % End:
    
    \begin{document}\begin{CJK*}[dnp]{JIS}{min}
    I can write
    E<ccedil>E<sect>E<129>E<atilde>E<129>E<macr>E<atilde>E<130>E<shy>E<atilde>E<131>E<copy>E<atilde>E<131>E<sup3>E<atilde>E<129>E<sect>E<atilde>E<129>E<153>
    \end{CJK*}
    \end{document}

see also __my-japanese-shapka__ in __.emacs__

__ UPDATE: __ Sometimes this does not work, but instead this works:

    \usepackage{CJKutf8}
    \begin{CJK*}{UTF8}{min}

These 2 things are complimentary to each other, i.e. it seems
that if one does not work, then the other one does (hopefully).
When I copied and pasted  Kanji from emacs to Tk, and then back,
after that the first method does not work any more, but the
second one does.


