#!/bin/bash

rm build-qt5msgr-Desktop-Debug/*

cd build-qt5msgr-Desktop-Debug

QT_SELECT=qt5 qmake -makefile ../qt5msgr/

make

cd ..
