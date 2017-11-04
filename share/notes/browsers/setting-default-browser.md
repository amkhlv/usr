Setting default browser
=======================

As root:

    update-alternatives --config x-www-browser

    aptitude install gconftool
    aptitude install gvfs-bin
    
Then, as an ordinary user:

    gconftool --type string -s /desktop/gnome/url-handlers/http/command  "/usr/bin/vivaldi %s"
    gconftool --type string -s /desktop/gnome/url-handlers/https/command "/usr/bin/vivaldi %s"

    gvfs-mime --set x-scheme-handler/http  vivaldi-stable.desktop
    gvfs-mime --set x-scheme-handler/https vivaldi-stable.desktop

