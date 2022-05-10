Introduction
============

I use basically the [UEFI GRUB](https://wiki.debian.org/GrubEFIReinstall) bootloader, 
but to avoid "boot-coups" I chainload it with [rEFInd](http://www.rodsbooks.com/refind/)


Create EFI partition on GPT disk
================================

It should have size `512M`, be formatted as `vfat`, and have GPT label `ef00` .


Install rEFInd
===================

Mount `/boot`

    aptitude install refind

And then:

    refind-install --usedefault /dev/sdxY

where `/dev/sdxY` is the `EFI` partition. 
The `--usedefault` flag means that `Refind` will be the default to boot.
For that to be true, the executable and accompanying files should all go to `EFI/BOOT/`.
Moreover, the executable will be called `bootx64.efi`; it is just a copy of `refind_x64.efi`.

__If__ need to copy drivers:

    cp /usr/share/refind/refind/drivers_x64/* EFI/BOOT/drivers_x64/

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

Or, `OpenShell.efi` can be obtained from [OpenCore bootloader](https://github.com/acidanthera/OpenCorePkg),
by downloading release `zip` and copying `X64/EFI/OC/Tools/OpenShell.efi` to `EFI/tools/`.
(See [What the Heck is OpenCore Bootloader](https://kextcache.com/what-the-heck-is-opencore-bootloader-and-what-it-means-to-hackintosh-community/).)

### Preliminary commands

A nice description is at [Arch wiki](https://wiki.archlinux.org/index.php/Unified_Extensible_Firmware_Interface#Important_UEFI_Shell_commands)

UEFI Shell commands usually support `-b` option which makes output __pause after each page__. Run `help -b` to list available commands.

`mode` Displays or changes the console output device mode. When this command is used without any parameters, it shows the list of availabel modes.
If two arguments given, sets to number of cols and rows, _e.g._ :

    mode 80 50

`map` displays a list of device mappings _i.e._ the names of available file systems (_e.g._ `fs0`) and storage devices (_e.g._ `blk0`)

__Before running file system commands__ such as cd or ls, you need to change the shell to the appropriate file system by typing its name:

    Shell> fs0:
    fs0:\> cd EFI\BOOT
    
__PATH separator is \ and not / !!!__ (important for auto-completion)

### Basic commands

1. `bcfg` modifies the UEFI NVRAM entries which allows the user to change the boot entries or driver options; see `help bcfg -v -b`

2. `edit` provides a basic text editor with an interface similar to `nano`

3. `type` to print a file to the screen

4. `ver` Displays the version information for the UEFI Shell and the underlying UEFI firmware.

5. `time [hh:mm[:ss]] [-tz tz] [-d dl]` Displays or sets the current time for the system.

6. `reset -s` does __shutdown__ ; `reset -c` does cold reboot ; `reset -w` does warm reboot

### Any .efi executable can be called

just by typing its name and pressing `ENTER`. 

### Booting Linux

`EFISTUB` is the mechanism to boot by executing Linux kernel as an EFI executable. Something like this:

    \vmlinuz-linux root=PARTUUID=...  rw  initrd=\initramfs-linux.img

