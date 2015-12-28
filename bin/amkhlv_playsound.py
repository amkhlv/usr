#!/usr/bin/env python3
#coding=utf-8  

import pyaudio  
import wave
import os
import sys
import argparse
import yaml

DEBUG = False 

def help_message(name=None):
    return """
___________________________________________________________________________

My beeper. 
It reads a command from the pipe and plays the corresponding WAV.

The first argument should be the path to the YAML configuration file, e.g.:

""" + os.path.basename(__file__) + """ /path/to/my/conf.yaml

where conf.yaml  should have the following structure:

pipe: /path/to/my/soundpipe.fifo
commands:
  drum: /home/user/drum.wav
  bell: /home/user/bell.wav

The ``exit'' command terminates the execution 
(and should never be used as a name of a command in the configuration file)
___________________________________________________________________________
"""

parser = argparse.ArgumentParser(
                                 usage=help_message(), 
                                 formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument("yamlpath", help="path to the configuration file")

def read_yaml(yaml_filename):
    """
    To read data from a yaml file

    :param str yaml_filename:
    :return: dict
    :rtype: dict
    """
    yamfl = open(yaml_filename, 'r')
    y = yaml.safe_load(yamfl)
    yamfl.close()
    return y

def mydebug(x):
    if DEBUG:
        with open("/tmp/amkhlv-beeper-log.txt",'a') as fh:
            fh.write(x)
    else: pass

def play(filename, pya):
    mydebug("playing: " + filename + "\n")
    #open a wav format music 
    f = wave.open(filename,"rb")  
    #instantiate PyAudio
    stream = pya.open(format = pya.get_format_from_width(f.getsampwidth()),
                      channels = f.getnchannels(),
                      rate = f.getframerate(),
                      output = True)
    chunk = 1024
    data = f.readframes(chunk)
    while len(data)>0:
        stream.write(data)
        data = f.readframes(chunk)
    mydebug("Ended writing to stream\n")
    stream.stop_stream()
    stream.close()

if __name__ == '__main__':
    args = parser.parse_args()
    conf = read_yaml(args.yamlpath)
    p = pyaudio.PyAudio()
    try:
        os.mkfifo(conf['pipe'])
    except OSError as e:
        sys.stderr.write("Failed to create FIFO: %s" % e)
        sys.stderr.flush()
    while True:
        with open(conf['pipe']) as fh:
            mydebug("Listening on the pipe " + conf['pipe'] + "\n")
            x = fh.read().rstrip()
            if x in conf['sounds'].keys():
                play(conf['sounds'][x],p)
            elif x == "exit":
                break
    p.terminate()
