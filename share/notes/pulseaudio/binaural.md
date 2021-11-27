Binaural Headphones
===================

    aptitude install bs2b-ladspa

    pacmd load-module module-ladspa-sink sink_name=binaural master=alsa_output.pci-0000_00_1f.3.analog-stereo plugin=bs2b label=bs2b control=700,4.5


where `master` is obtained by running:

    pacmd list-sinks |grep name:

which should return something like:

    name: <alsa_output.pci-0000_00_1f.3.analog-stereo>

