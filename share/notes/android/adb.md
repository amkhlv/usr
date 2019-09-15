Permissions
===========

User should be in the `plugdev` group:

    usermod -a -G plugdev andrei

Enabling
========

First have to enable `developer options` by tapping 9 times on Build Number in Adroid Info.

Then the section `Developer Options` will appear in `Settings`, with `Enable ADB` option.

Also, in `Developer Options` enable `Root acess` as `adb only`

This command should now list the phone:

    adb devices

__Dont forget to DISABLE ADB after__

Using
=====

Root mode
---------

Usually need to restart as `root`, to enable root access:

    adb root

Shell
-----

    adb shell

File transfer
-------------

`adb pull` and `adb push`


Where updates are stored?
=========================

    /data/data/org.lineageos.updater/app_updates/

or

    /data/lineageos_updates/

Connecting over network
=======================

This is useful when running [Android x86](http://www.android-x86.org) in QEMU.
Just execute, at the very beginning:

    adb connect 192.168.xxx.x
