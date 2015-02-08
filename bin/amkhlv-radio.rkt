#!/usr/bin/env racket

#lang racket/gui

#|
Copyright 2012 Andrei Mikhailov.  This program is Free Software; you
can redistribute it and/or modify it under the terms of the GNU Lesser General
Public License as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.  This program is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See http://www.gnu.org/licenses/ for details.  For other
licenses and consulting, please contact the author.
|#

(define-syntax-rule (station name url)
  (unless
      (new button% 
           [parent frame]
           [label name]
           [callback (lambda (button event)
                       (subprocess #f #f #f (string->path "/usr/bin/vlc") url)
                       (exit))])
    (raise 'failed-to-create-button)))

(define frame (new frame% [label "Radio stations"]))
(send frame show #t)


(station "Radio BACH" "http://streaming208.radionomy.com:80/Radio-Bach")

(station "Air Classique" "http://streaming201.radionomy.com:80/Air-Classique")

(station "Radio Swisse"  "http://zlz-stream12.streaming.init7.net/1/rsc_fr/mp3_128")

(station "WCPE" "http://audio-ogg.ibiblio.org:8000/wcpe.ogg.m3u")

(station "WQXR (New York)" "http://www.wqxr.org/stream/wqxr/mp3.pls")
(station "WQXR (New York) --- RTSP" "rtsp://wnyc-3gp.streamguys.com/wqxr/wqxr.sdp")

(station "Classical New England WGBH" "http://streams.wgbh.org/streamb/classical.m3u")

(station "Klassieke Muziek" "http://www.concertzender.nl/streams/klassiek.att")
(station "Oude Muziek" "http://www.concertzender.nl/streams/oudemuziek.att")
(station "Gregoriaans" "http://www.concertzender.nl/streams/gregoriaans.att")
