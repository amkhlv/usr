Location of keyring files and configuration files in Debian
===========================================================

They are all in `~/.local/share/keyrings/` . That directory contains __both__ configuration files and keystore files; 
keystore files have the extention `.keystore`.

There are several of those "keystores", and one of them is considered "standard", the one which `Evolution` etc use.
Which one is standard? It is specified in the __configuration__ file which is called `default`. For example, if the
content of this file is a single line:

    myname

then the standard keystore will be: `myname.keystore`



