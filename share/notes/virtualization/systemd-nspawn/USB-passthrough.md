The `nspawn` config should contain:

    Bind=/dev/bus/usb:/dev/bus/usb

Besides, to allow the container the access to the device, need (wth running container):

    echo 'c 189:SMTH rwm' > /sys/fs/cgroup/devices/machine.slice/machine-MACHINENAME.scope/devices.allow

where `SMTH` is obtained by running:

    file /dev/bus/usb/MMM/NNN

It will reply:

    /dev/bus/usb/MMM/NNN: character special (189/SMTH)

--- that `SMTH` is the `SMTH` (a number)



