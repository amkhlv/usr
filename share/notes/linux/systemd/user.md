
Failed to get D-Bus connection
==============================

Then add these lines to `~/.bashrc` :

    export XDG_RUNTIME_DIR="/run/user/$UID"
    export DBUS_SESSION_BUS_ADDRESS="unix:path=${XDG_RUNTIME_DIR}/bus"


