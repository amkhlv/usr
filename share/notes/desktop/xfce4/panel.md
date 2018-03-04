Add custom launcher
===================

Edit file `~/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-panel.xml` and in the list of `<property name="plugins">` add:

    <property name="plugin-7" type="string" value="launcher"/>

(the "7" in "plugin-7" is just the next available number)

Then `mkdir ~/.config/xfce4/panel/launcher-7` and `ln -s` your `.desktop` file there

