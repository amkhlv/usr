#!/usr/bin/env python  
#coding=utf-8  

import pyaudio  
import wave
import os


FIFO="/tmp/amkhlv-soundpipe.fifo"
#define stream chunk
chunk = 1024  

def play(filename, pya):
    #open a wav format music 
    f = wave.open(filename,"rb")  
    #instantiate PyAudio
    stream = pya.open(format = pya.get_format_from_width(f.getsampwidth()),
                      channels = f.getnchannels(),
                      rate = f.getframerate(),
                      output = True)
    data = f.readframes(chunk)
    while data != '':
        stream.write(data)
        data = f.readframes(chunk)
    stream.stop_stream()
    stream.close()


if __name__ == '__main__':
    p = pyaudio.PyAudio()
    while True:
        with open(FIFO) as fh:
            x = fh.read().rstrip()
            if x == "drum":
                play("/home/andrei/a/sounds/drum-1.wav",p)
            elif x == "exit":
                break
    p.terminate()
