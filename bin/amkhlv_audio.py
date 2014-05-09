#!/usr/bin/env python3

usage = """

 %prog [options]

Notice that there are Linux commands:

  arecord --list-devices
  aplay --list-devices

which give information about the available sound cards
"""

from optparse import OptionParser
from os import system, getenv
import sys
import pyaudio, wave



BUFFER_SIZE = 1024
FORMAT = pyaudio.paInt16
CHANNELS = 1


#init sound stream
def record(pa, howlong, input_device_index, rate, filename):

    dev_params = pa.get_device_info_by_index(input_device_index)

    print(rate)
    
    stream = pa.open(
        format = FORMAT,
        input = True,
        channels = CHANNELS,
        rate = rate ,
        input_device_index = input_device_index,
        frames_per_buffer = BUFFER_SIZE
    )
    #run recording
    print('Recording...')
    data_frames = []
    for f in range(0, int(rate/BUFFER_SIZE) * howlong
):
        try:
            data = stream.read(BUFFER_SIZE)
            data_frames.append(data)
            sys.stdout.write("x")
        except IOError as err:
            print("I/O error: {0}".format(err))
            sys.stdout.write("?")
    print('Finished recording...')
    stream.stop_stream()
    stream.close()


    wf = wave.open(filename, 'wb')
    wf.setnchannels(CHANNELS)
    wf.setsampwidth(pa.get_sample_size(FORMAT))
    wf.setframerate(rate)
    wf.writeframes(b''.join(data_frames))
    wf.close()



if __name__ == '__main__':
    parser = OptionParser(usage=usage)

    parser.add_option("-r", "--record", dest="do_record", default=False, action="store_true",
                      help="""record""")
    parser.add_option("-f", "--file", dest="filename", help="filename")
    parser.add_option("-d", "--device", type="int", dest="device_index", help="device index")
    parser.add_option("-i", "--info", dest="get_info", default=False, action="store_true", 
                      help="""get device info""")
    parser.add_option("-l", "--list", dest="list_devices", default=False, action="store_true", help="""list devices""")
    parser.add_option("--rate", type="int", dest="rate", help="bit rate; this should be detected automatically; but if you get tons of [Errno Input overflowed] errors, then try to set this to 32000")
    parser.add_option("-s", "--seconds", dest="time_seconds", type="int", default=3, metavar="SECONDS", help="how many seconds to record")

    (options, args) = parser.parse_args()
    pa = pyaudio.PyAudio()
    if options.do_record:
        if options.rate: 
            rate = options.rate
        else: 
            dev_params = pa.get_device_info_by_index(options.device_index)
            rate = int(dev_params['defaultSampleRate'])
        record(pa, options.time_seconds, options.device_index, rate, options.filename)
    if options.get_info:
        print(pa.get_device_info_by_index(options.device_index))
    if options.list_devices:
        ranout=False; j=0;
        while not(ranout):
            try:
                print(str(j) + ": " + pa.get_device_info_by_index(j)['name'])
                j = j+1
            except IOError:
                print("--------------")
                ranout=True

    pa.terminate()    
