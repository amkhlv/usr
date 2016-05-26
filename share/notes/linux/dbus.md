GUI
===

    aptitude install d-feet


List available services
=======================

Session:

    dbus-send --session           \
    --dest=org.freedesktop.DBus \
    --type=method_call          \
    --print-reply               \
    /org/freedesktop/DBus       \
    org.freedesktop.DBus.ListNames

System:

    dbus-send --system            \
    --dest=org.freedesktop.DBus \
    --type=method_call          \
    --print-reply               \
    /org/freedesktop/DBus       \
    org.freedesktop.DBus.ListNames
