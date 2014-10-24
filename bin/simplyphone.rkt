#!/usr/bin/env racket

#lang racket

#|
Script for launching the gstreamer streams through UDP ports.
This is a simplistic replacement to more advanced methods such as Ekiga, Linphone, SFLPhone, ...
In practice this solution requires VPN.

Requires gst-launch (which on Debian is in   gstreamer-tools  )

The configuration file: ~/.config/amkhlv/simplyphone.xml , the conf file should contain lines similar to the following:

<conf>
  <others>
    <person name="Alice">audio/x-raw-int,rate=8000,channels=1 ! mulawenc ! rtppcmupay ! udpsink host=192.168.10.21 port=8888</person>
    <person name="Barbara">audio/x-raw-int,rate=8000,channels=1 ! mulawenc ! rtppcmupay ! udpsink host=192.168.10.23 port=8888</person>
    <person name="Carol">audio/x-raw-int,rate=8000,channels=1 ! mulawenc ! rtppcmupay ! udpsink host=192.168.10.25 port=8888</person>
  </others>
  <source>alsasrc</source>
  <sink>pulsesink</sink>
  <in-port>8888</in-port>
  <in-args>caps="application/x-rtp,clock-rate=(int)8000" ! queue ! rtppcmudepay ! mulawdec ! audioconvert</in-args>
</conf>

Starting this program will bring up the popup window and open the incoming audio-stream.
To open one or more outgoing audio-streams, toggle on the corresponding checkbox.
Toggling off that checkbox later will close the outgoing stream.
Killing the window or pressing "Stop and EXIT" button will close the incoming stream, and all remaining
outgoing streams. 
|#

(require racket/gui/base xml xml/path)

(define gst-launch (find-executable-path "gst-launch"))
(unless gst-launch (error "You need to install the program called gst-launch"))

(define config   ;parsing the configuration file:
  (let* ([inport 
          (open-input-file 
           (build-path (find-system-path 'home-dir) ".config" "amkhlv" "simplyphone.xml")
           #:mode 'text)]
         [root (document-element (read-xml inport))]
         )
    (close-input-port inport)
    (xml->xexpr root)))
(define list-of-others (se-path*/list '(others) config))
(define sound-source   (se-path* '(source) config))
(define sound-sink     (se-path* '(sink) config))
(define sound-in-port  (se-path* '(in-port) config))
(define stream-in-args (se-path* '(in-args) config))

(define streamin-proc  ;starting the input stream:
  (let*-values 
      (
       [(all-flags) 
        (cons "udpsrc" 
              (cons (string-append "port=" sound-in-port)
                    (append (string-split stream-in-args) (list "!" sound-sink))))]
       [(sp inport outport errport)
        (apply subprocess (current-output-port) #f 'stdout gst-launch all-flags)]
       )
    (close-output-port outport)
    (display (string-append "started input stream:  gst-launch " (string-join all-flags " ") "\n"))
    sp))

(define streams (make-hash)) ;the global hash for output streams:

(define frame (new frame% [label "simplyphone"])) ;preparing GUI
(for ([o list-of-others])
  (let (
        [fullname (se-path* '(person #:name) o)]
        [flags    (se-path* '(person) o)]
        )
    (when fullname 
      (define (toggle-streamout cb ev)
        (if (send cb get-value)
            (let*-values (
                          [(all-flags) (cons sound-source (cons "!" (string-split flags)))]
                          [(sp inport outport errport)
                           (apply subprocess (current-output-port) #f 'stdout gst-launch all-flags)]
                          )
              (when inport (close-input-port inport))
              (close-output-port outport)
              (when errport (close-input-port errport))
              (display (string-append "started output stream:   gst-launch " (string-join all-flags " ") "\n"))
              (hash-set! streams fullname sp))
            (begin 
              (display "stopped gstream\n")
              (let ([s (hash-ref streams fullname)])
                (hash-remove! streams fullname)
                (subprocess-kill s #t)))
            ))
      (new check-box%
           [parent frame]
           [label fullname]
           [value #f]
           [callback toggle-streamout]
           ))
    ))

(define exit-button 
  (new button% 
       [label "Stop and EXIT"] 
       [parent frame]
       [callback (lambda (b e) (send frame show #f))]
       )
  )

(send frame show #t) ;starting GUI:

(yield 'wait) ;waiting until user kills the window;

(for ([k (hash-keys streams)]) ;stopping the incoming audio stream:
  (display (string-append "killing the outgoing stream to " k "\n"))
  (subprocess-kill (hash-ref streams k) #t))
(display "killing the incoming stream\n")
(subprocess-kill streamin-proc #t)
