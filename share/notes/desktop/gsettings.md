GSettings
=========

Setting custom colors
---------------------

    gsettings set org.gtk.Settings.ColorChooser custom-colors '[(1.0, 0.89411764705882357, 0.88235294117647056, 1.0), (0.47846734234234256, 0.9966666666666667, 0.18936666666666663, 1.0)]'

(The last `1.0` is probably alpha-channel or something)

We could have discovered this:

    gsettings list-schemas | grep -i color

    gsettings list-keys org.gtk.Settings.ColorChooser

    gsettings get org.gtk.Settings.ColorChooser custom-colors 


