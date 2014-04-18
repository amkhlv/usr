#!/bin/bash
# echo A
# rm -rf ~/a/homepage/htmldocs/*
# rm -rf ~/a/homepage/htmldocs/.*
# echo B
# ( cd $HOME ;
# find a/ -type d -regex '.*/html-doc$' -not -path 'a/homepage/htmldocs/*' -not -path 'a/html-doc' | while read N ;  do
#     echo $N ;
#     showname="$( echo $( dirname "$N" ) | sed -e's/\//./g' | sed -e's/^a\.//' )"
#     # mkdir -p ~/a/homepage/htmldocs/"$showname" ;
#     ln -s "$HOME/$N" ~/a/homepage/htmldocs/"$showname" ;
# done
# )

echo '<HTML>' > ~/a/homepage/html-docs.html

find ~/a/ -type d -regex '.*/html-doc$' | sed -e's/\/html-doc$//' | sort | 
  while read N
  do
    echo '<a href=file://'"$N/html-doc"'>'"$( echo "$N" | sed -e"s|^"$HOME"/||" )"'</a><br><br>' >> ~/a/homepage/html-docs.html
  done

echo '</HTML>' >> ~/a/homepage/html-docs.html

( 
  cd ~/a/scribble-head/
  scribble main.scrbl
)
