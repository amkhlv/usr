# Dependencies

    aptitude install pamusb-tools libpam-usb

# CHECKLIST

__NOTE:__ There was anomaly: for some reason the `/etc/fstab` got automount
lines. __Check /etc/fstab after doing pamusb-conf__!

- Format flashka to `ext2` and do AS ROOT:

        pamusb-conf --add-device amkhlv-unlock-andrei
        pamusb-conf --add-device amkhlv-unlock-root
        pamusb-conf --add-user andrei
        pamusb-conf --add-user root

    (the last line is because I also want to execute the command `su`).

- Edit `/etc/pamusb.conf` so that it contains:

        <user id="andrei">
                <device>amkhlv-unlock-andrei</device>
                <option name="pad_expiration">0</option>
        </user>
        <user id="root">
                <device>amkhlv-unlock-root</device>
                <option name="pad_expiration">0</option>
        </user>

    with such settings, we will have the one-time pads on the USB regenerated every time

- Execute as `andrei`:

        pamusb-check andrei

- Execute as `root`:

        pamusb-check root

- Edit `/etc/pam.d/other` ; it should be:

        auth     required       pam_deny.so
        account  required       pam_deny.so
        password required       pam_deny.so
        session  required       pam_deny.so

- Proceed to [pam-auth-update](pam-auth-update.md)

# Repairing one-time pads

Just remove the `~/.pamusb` and then:

    pamusb-check andrei

should give:

       * Authentication request for user "andrei" (pamusb-check)
       * Device "amkhlv-unlock" is connected (good).
       * Performing one time pad verification...
       * Regenerating new pads...
       * Access granted.

This also regenerates new 1-time pads (as it says). The pads are in `~/.pamusb/`.
