I am not sure if I actually need the autologin feature.

# For `lightdm`

## Enable autologin

Look up these lines in `lightdm` configuration file, uncomment them and customize to your preference.

    [SeatDefaults]
    #autologin-user=
    #autologin-user-timeout=0

## Secure autologin

Now we have to make sure that the autologin only happens when the USB key is inserted

Replace

    auth    required        pam_permit.so

with

    auth    sufficient      pam_usb.so

in `/etc/pam.d/lightdm-autologin`
