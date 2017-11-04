How to handle custom protocols
==============================

I want the browser to call my command on clicking the `tel:` link.

As [explained here](people.w3.org/~dom/archives/2005/09/integrating-a-new-uris-scheme-handler-to-gnome-and-firefox/)
the browser uses the generic gnome-open command to handle unknown URI schemes. So, we need to configure `gnome`:

    gconftool-2 -t string -s /desktop/gnome/url-handlers/tel/command /usr/local/lib/amkhlv/tel-handler.sh
    gconftool-2 -s /desktop/gnome/url-handlers/tel/needs_terminal false -t bool
    gconftool-2 -t bool -s /desktop/gnome/url-handlers/tel/enabled true

