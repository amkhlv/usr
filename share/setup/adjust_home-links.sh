#!/bin/bash

mkdir ~/aa
mkdir ~/usr ~/usr/local ~/usr/local/lib
[ -d ~/usr/lib ] || ln -s ~/a/other/usr/lib ~/usr/lib
[ -d ~/usr/man ] || ln -s ~/a/other/usr/man ~/usr/man
[ -d ~/usr/bin ] || ln -s ~/a/other/usr/bin ~/usr/bin
[ -d ~/usr/share ] || ln -s ~/a/other/usr/share ~/usr/share

mkdir ~/mo ~/mo1 ~/mo2 ~/mo3
mkdir ~/msdos
mkdir ~/pny8

[ -d ~/maildirs ] || ln -s ~/a/maildirs ~/maildirs
ln -s ~/usr/lib/emacs/emacs.el ~/.emacs

[ -d "~/.local" ]   || mkdir ~/.local

ln -s ~/a/other/home/.m2    ~/.m2
ln -s ~/a/other/home/.ivy2  ~/.ivy2
ln -s ~/a/other/home/.IdeaIC14 ~/.IdeaIC14

ln -s ~/a/other/IdeaProjects ~/IdeaProjects

[ -d ~/.amkhlv-emails ] || mkdir ~/.amkhlv-emails 
