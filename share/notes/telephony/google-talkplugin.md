Installation
============

First add the Google repository:

    wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
    echo deb http://dl.google.com/linux/talkplugin/deb/ stable main >> /etc/apt/sources.list
    aptitude update

Then install:

    aptitude install google-talkplugin

Then need to run:

    dpkg-reconfigure google-talkplugin

After that, it runs in `Firefox`. But __dont forget to activate__ it in `Add-ons` → `Plugins`



