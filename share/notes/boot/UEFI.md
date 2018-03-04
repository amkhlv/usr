Introduction
============

I use basically the [UEFI GRUB](https://wiki.debian.org/GrubEFIReinstall) bootloader, 
but to avoid "boot-coups" I chainload it with [rEFInd](http://www.rodsbooks.com/refind/)


Create EFI partition on GPT disk
================================

It should have size `512M`, be formatted as `vfat`, and have GPT label `ef00` .


First install GRUB with UEFI target
===================================

    aptitude install grub-efi

(this is incompatible with `grub-pc`, so `aptitude` will suggest the removal of `grub-pc`)

On pendrive:

    grub-install -v --target x86_64-efi --efi-directory=X --boot-directory=Y --removable /dev/sdX


Then install rEFInd
===================

Mount `/boot`

    aptitude install refind

And then:

    refind-install --usedefault /dev/sdxY

where `/dev/sdxY` is the `EFI` partition.

Then mount `/dev/sdxY` somewhere and go there. The root will contain the folder named `EFI`.
In `EFI/BOOT` find `refind.conf` and edit it as [described here](http://www.rodsbooks.com/refind/configfile.html). 
Namely, it should contain the stanza:


    menuentry "MySystem" {
        icon EFI/BOOT/icons/os_clover.png
        volume xxxxxxxx-yyyy-zzzz-wwww-nnnnnnnnnnnn
        loader grub/x86_64-efi/grub.efi 
    }

where `xxxxxxxx-yyyy-zzzz-wwww-nnnnnnnnnnnn` is the `PARTUUID` of the `/boot`.
Notice that in the first line `EFI/BOOT/icons/os_clover.png` is relative to the root of the `EFI` partition.
Then we "cd into volume" `/boot` and in the third line the  `loader` path is relative to the root of `/boot`.
(There is the file `grub.efi` there because we configured `grub` for `EFI`.) Therefore, the `MySystem` stanza
tells `EFI` to load `GRUB`. Then, the standard `GRUB` menu will appear (`grub.efi` knows where it is, so it
knows where to find the `GRUB` config menu).

Now __unmount__ the `EFI` partition.

Maybe I need to run `refind-install --usedefault /dev/sdxY` again after that; I am not sure...


Autodetected kernels
====================

rEFInd also autodetects kernels and presents them to the menu. When pressed on such a menu item, the kernel is loaded "directly",
_i.e._ as a EFI binary. But how to pass kernel parameters? Create a file `refind_linux.conf` in the same folder as the kernel-to-be-autodetected.
The file [should contain lines](https://wiki.archlinux.org/index.php/REFInd) like this:

    "Boot using default options"     "root=UUID=XXXXXXXX ro quiet"


REFInd addons
=============

EFI shell
---------

### Setting up

EFI shell can be [downloaded at tianocore](https://github.com/tianocore/edk2/blob/master/ShellBinPkg/UefiShell/X64/Shell.efi).
In order to be recognized by `rEFInd` it should be moved to `EFI/tools/shell.efi`.

### Preliminary commands

A nice description is at [Arch wiki](https://wiki.archlinux.org/index.php/Unified_Extensible_Firmware_Interface#Important_UEFI_Shell_commands)

UEFI Shell commands usually support `-b` option which makes output __pause after each page__. Run `help -b` to list available commands.

`mode` Displays or changes the console output device mode. When this command is used without any parameters, it shows the list of availabel modes.
If two arguments given, sets to number of cols and rows, _e.g._ :

    mode 80 50

`map` displays a list of device mappings _i.e._ the names of available file systems (_e.g._ `fs0`) and storage devices (_e.g._ `blk0`)

__Before running file system commands__ such as cd or ls, you need to change the shell to the appropriate file system by typing its name:

    Shell> fs0:
    fs0:\> cd EFI/

### Basic commands

1. `bcfg` modifies the UEFI NVRAM entries which allows the user to change the boot entries or driver options; see `help bcfg -v -b`

2. `edit` provides a basic text editor with an interface similar to `nano`

3. `type` to print a file to the screen

4. `ver` Displays the version information for the UEFI Shell and the underlying UEFI firmware.

5. `time [hh:mm[:ss]] [-tz tz] [-d dl]` Displays or sets the current time for the system.

6. `reset -s` does __shutdown__ ; `reset -c` does cold reboot ; `reset -w` does warm reboot

### Any .efi executable can be called

just by typing its name and pressing `ENTER`. For example `EFI/BOOT/bootx64.efi` will just boot the operating system.
