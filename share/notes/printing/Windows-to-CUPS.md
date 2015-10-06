Printing from Windows to CUPS
=============================

This is explained [here](https://superuser.com/questions/296718/printing-from-windows-to-ubuntu-shared-printer#)

No Samba!

Strangely, I have to choose `Select a shared printer by name` instead of `Add a printer using a TCP/IP`

Also, interestingly, I have to select `Generic` manufacturer and `MS Publisher Imegesetter` printer driver, which will just have Windows send print jobs to CUPS in PostScript format and allow CUPS to use its driver instead.

