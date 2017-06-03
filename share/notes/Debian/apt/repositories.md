# Experimental

This is for getting the latest version of `Firefox`:

    deb http://ftp.br.debian.org/debian/ experimental main

# Unofficial repositories

[https://wiki.debian.org/UnofficialRepositories](https://wiki.debian.org/UnofficialRepositories)

## Google talk plugin

To install the Google signing key:

    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -

The line in `/etc/apt/sources.list`

    deb http://dl.google.com/linux/talkplugin/deb/ stable main
