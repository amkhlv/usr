# Bibliography section in LaTeX file

    \bibliographystyle{JHEP} \renewcommand{\refname}{Bibliography}
    \addcontentsline{toc}{section}{Bibliography}
    \bibliography{../andrei}

Explanations:

1. `JHEP` in the first line is the name of the style. There must be the corresponding file `JHEP.bst` in the current directory
2. `../andrei` in the third line means that there is a file `andrei.bib` in the parent dir.

# bibtex commands

Suppose that I have a file: `myfile.tex` :

    pdflatex myfile (doesn't require .tex extension)
    bibtex myfile (doesn't require  extension)
    pdflatex myfile
    pdflatex myfile

Then, take the file `filename.bbl` and copy its content to the end of `filename.tex`
