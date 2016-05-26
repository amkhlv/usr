
Failed to get D-Bus connection
==============================

libpam-systemd
--------------

Make sure that `libpam-systemd` is installed


Enable lingering
----------------

"Lingering" means persistent user instance `systemd`. It should be activated:

    sudo loginctl enable-linger andrei

(to verify: `sudo loginctl show-user andrei` should list `Linger=yes`)


XDG_RUNTIME
-----------

And add this line to `~/.bashrc` :

    export XDG_RUNTIME_DIR="/run/user/$UID"

and sometimes also this:

    export DBUS_SESSION_BUS_ADDRESS="unix:path=${XDG_RUNTIME_DIR}/bus"


