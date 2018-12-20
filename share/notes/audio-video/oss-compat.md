# OSS compatibility

[OSS](http://en.wikipedia.org/wiki/Open_Sound_System) is considered obsolete, but it is very handy.

For example, I want to have `/dev/dsp` where I can cat the `.wav` files.

I create it like this:

    aptitude install oss-compat 
    modprobe snd-pcm-oss 

Then for example:

    ssh ... "cat /dev/dsp" > /dev/dsp

will allow to listen to the remote computer 
