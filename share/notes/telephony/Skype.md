Alternatives to Skype
=====================

Skype is difficult to install and use. Some alternatives:

SIP
---

The main problem with using SIP is firewall traversal.
There is a very nice VOIP service called
[Diamond worldwide communication service](https://www.diamondcard.us/).
They provide an `openvpn` VPN service, which typically works from inside the firewall (unless if you have a
partucularly draconian firewall, which e.g. blocks UDP). They allow to call to ordinary phones.
See [my writeup on Ekiga](ekiga.md).

Another problem is low quality of existing client programs. The least bad are Ekiga, Linphone and SFLphone.
It seems that DiamondCard is working on their own client.

Google talk
-----------

See [my writeup](google-talkplugin.md).
It works in the browser from GMail. Unfortunately, it seems to be phased out in favor of "Google Circles".
Those "Google Circles" have some very confusing interface, I was not able to figure out how to use them. 


Installing Skype
================

Skype only works on 32 bit systems.

I could not find a secure download link, so the download is over `http`:

    wget -O skype-install.deb http://www.skype.com/go/getskype-linux-deb

Maybe compute `sha256sum` and
Google for it. This, at least, would show that other people have also downloaded the same file.
I think it requires `pulseaudio`, but this is standard now.

    dpkg -i skype-install.deb

will report errors; to fix, say:

    apt-get -f install

Skype is [suspected spyware](http://techrights.org/wiki/index.php/Skype_is_Spy_Campaign).


Configuring
===========

Set all sound devices to PulseAudio Server.
