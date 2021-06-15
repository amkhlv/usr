Adjusting document margins
==========================

(In other words, shifting the page both horizonthally and vertically.)

This can be done using the program `pdfjam` :

     pdfjam --a4paper --offset "-2cm 2cm" taxa-140120.pdf


Splitting into pages
====================

Use `pdftk` :

     pdftk in.pdf burst

--- this creates files named `pg_nnnn.pdf` and also creates the file `doc_data.txt`

Convert to SVG
==============

    aptitude install pdf2svg

Use it to convert multipage `pdf` to many `svg` files:

    pdf2svg  lista.pdf  svgs/lista_p%d.svg  all


