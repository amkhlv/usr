Install GRUB with UEFI target
=============================

    aptitude install grub-efi

(this is incompatible with `grub-pc`, so `aptitude` will suggest the removal of `grub-pc`)

On pendrive:

    grub-install -v --target x86_64-efi --efi-directory=X --boot-directory=Y --removable /dev/sdX

