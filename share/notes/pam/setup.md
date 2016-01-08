# Lock and unlock by the USB key

## Dependencies

    aptitude install pamusb-tools libpam-usb

## Basic conf

Instructions are taken from [here](http://linuxconfig.org/linux-authentication-login-with-usb-device).

## \*\*\* CHECKLIST \*\*\*

__NOTE:__ There was anomaly: for some reason the `/etc/fstab` got automount
lines. __Check /etc/fstab after doing pamusb-conf__!

- Format flashka to `ext2` and do AS ROOT:

        pamusb-conf --add-device amkhlv-unlock
        pamusb-conf --add-user andrei
        pamusb-conf --add-user root  

    (the last line is because I also want to execute the command `su`).

- Edit `/etc/pamusb.conf` so that it contains:

                    <user id="andrei">
                            <device>amkhlv-unlock</device>
                            <option name="pad_expiration">0</option>
                    </user>
                    <user id="root">
                            <device>amkhlv-unlock</device>
                            <option name="pad_expiration">0</option>
                    </user>

    with such settings, we will have the one-time pads on the USB regenerated every time

- Add the following line to `/etc/pmount.allow`:

        /dev/disk/by-uuid/xxxxxxxxxxxxxxx

    where `xxxxxxxxxxxxxxx` is the UUID of the flashka partition on which the pads are

- Set the permissions on the `.pamusb` partition in flashka

    Mount the flashka and:

        chown root:andrei .pamusb
        chmod g+x .pamusb 
        chmod g+r .pamusb
        chmod g+w .pamusb

- Execute as `andrei`:

        pamusb-check andrei

- Execute as `root`:

        pamusb-check root

__If pamusb-check fails__, do:  `rm -rf ~/.pamusb/`
(for `andrei` or `root`, depending on which one failed).


## Repairing one-time pads

Just remove the `~/.pamusb` and then:

    pamusb-check andrei

should give:

       * Authentication request for user "andrei" (pamusb-check)
       * Device "amkhlv-unlock" is connected (good).
       * Performing one time pad verification...
       * Regenerating new pads...
       * Access granted.

This also regenerates new 1-time pads (as it says). The pads are in `~/.pamusb/`.
