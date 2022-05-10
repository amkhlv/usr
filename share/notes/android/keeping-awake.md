Keeping awake while connected to computer
=========================================

First solution
--------------

    while true; do adb shell input keyevent mouse ; sleep 30 ; done

Second solution
---------------

    adb shell
    svc power stayon usb

Third solution
--------------

    scrcpy -w
