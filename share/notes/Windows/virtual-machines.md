Free VirtualBox images for testing
==================================

May be available [here](https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/)

or [better here](https://developer.microsoft.com/en-us/windows/downloads/virtual-machines)

and who knows where else?

Your processor does not have 64bit
==================================

This happens even if processor does have 64bit enabled. 
First of all, `Windows 10 (64bit)` should be chosen as OS version in `Settings`.
But even if it is choosen, still this error may happen. If it happens
choose (falsely) `Windows 10 (32 bit)` and then try to boot it. It should 
give the same error. Then, change it back to `Windows 10 (64 bit)` and boot
again. This time, hopefully, it should boot OK.

Guest extensions
================

trivial, from the `VirtualBox`-supplied CD (click on `devices`, mount Guest Extensions...)

Shared folder
=============

First go to the `network` tab in File Manager, and enable network discovery.

In `PowerShell`:
    
    net use x: \\vboxsvr\MyChoosenName

It should appear under the `network` tab in File Manager, perhaps after refreshing.

Install programs
================

Run `PowerShell` as administrator, then:

    iwr https://chocolatey.org/install.ps1 -UseBasicParsing | iex

    choco install procexp -y

    choco install emacs -y

    choco install firefox -y


