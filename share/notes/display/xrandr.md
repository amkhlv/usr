# Setting resolution

Run the command:

    cvt 1600 900

to figure out the Modline for the reso,ution 1600x900 It should give something like this:

    Modeline "1600x900_60.00"  118.25  1600 1696 1856 2112  900 903 908 934 -hsync +vsync

Then say:

    xrandr --newmode "1600x900_60.00"  118.25  1600 1696 1856 2112  900 903 908 934 -hsync +vsync

Then say:

    xrandr --addmode default "1600x900_60.00"
    xrandr --output default --mode "1600x900_60.00"
