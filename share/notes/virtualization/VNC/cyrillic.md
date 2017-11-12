see also [my writeup on xkb](../../linux/keyboard.md)

Loading keysims
===============

I found [this interesting blog post](http://rus.akshaal.info/2008/02/vnc.html), suggesting
to add these lines: to `~/.vnc/xstartup` (but it does not seem to be needed):

    xmodmap -e "keycode 252 = Cyrillic_shorti Cyrillic_SHORTI"
    xmodmap -e "keycode 251 = Cyrillic_tse Cyrillic_TSE"
    xmodmap -e "keycode 250 = Cyrillic_u Cyrillic_U"
    xmodmap -e "keycode 249 = Cyrillic_ka Cyrillic_KA"
    xmodmap -e "keycode 248 = Cyrillic_ie Cyrillic_IE"
    xmodmap -e "keycode 247 = Cyrillic_en Cyrillic_EN"
    xmodmap -e "keycode 246 = Cyrillic_ghe Cyrillic_GHE"
    xmodmap -e "keycode 245 = Cyrillic_sha Cyrillic_SHA"
    xmodmap -e "keycode 244 = Cyrillic_shcha Cyrillic_SHCHA"
    xmodmap -e "keycode 243 = Cyrillic_ze Cyrillic_ZE"
    xmodmap -e "keycode 242 = Cyrillic_ha Cyrillic_HA"
    xmodmap -e "keycode 241 = Cyrillic_hardsign Cyrillic_HARDSIGN"
    xmodmap -e "keycode 240 = Cyrillic_ef Cyrillic_EF"
    xmodmap -e "keycode 239 = Cyrillic_yeru Cyrillic_YERU"
    xmodmap -e "keycode 238 = Cyrillic_ve Cyrillic_VE"
    xmodmap -e "keycode 237 = Cyrillic_a Cyrillic_A"
    xmodmap -e "keycode 236 = Cyrillic_pe Cyrillic_PE"
    xmodmap -e "keycode 235 = Cyrillic_er Cyrillic_ER"
    xmodmap -e "keycode 234 = Cyrillic_o Cyrillic_O"
    xmodmap -e "keycode 233 = Cyrillic_el Cyrillic_EL"
    xmodmap -e "keycode 232 = Cyrillic_de Cyrillic_DE"
    xmodmap -e "keycode 231 = Cyrillic_zhe Cyrillic_ZHE"
    xmodmap -e "keycode 230 = Cyrillic_e Cyrillic_E"
    xmodmap -e "keycode 229 = Cyrillic_io Cyrillic_IO"
    xmodmap -e "keycode 228 = Cyrillic_ya Cyrillic_YA"
    xmodmap -e "keycode 227 = Cyrillic_che Cyrillic_CHE"
    xmodmap -e "keycode 226 = Cyrillic_es Cyrillic_ES"
    xmodmap -e "keycode 225 = Cyrillic_em Cyrillic_EM"
    xmodmap -e "keycode 224 = Cyrillic_i Cyrillic_I"
    xmodmap -e "keycode 223 = Cyrillic_te Cyrillic_TE"
    xmodmap -e "keycode 222 = Cyrillic_softsign Cyrillic_SOFTSIGN"
    xmodmap -e "keycode 221 = Cyrillic_be Cyrillic_BE"
    xmodmap -e "keycode 220 = Cyrillic_yu Cyrillic_YU"

The above is <b><span style="color:red;">probably not needed</span></b>, but see [Foreign layout problems in main writeup](tigerVNC.md#cyrillic)

What is inet(...)?
==================

This is a big table of keysyms corresponding to "media keys" and other such fancy keys, which some keyboards have.
It is defined in the file:

    /usr/share/X11/xkb/symbols/inet

in its part: `xkb_symbols "evdev" {...}`. The point is, these keys are not used, and the corresponding keysym table should be better unloaded
(because it takes up space and prevents useful keysyms from being loaded, such as for example Cyrillic, defined in:

    /usr/share/X11/xkb/symbols/ru

Notice that `/usr/share/X11/xkb/symbols/ru` has a section `xkb_symbols "phonetic" {...}`

Look also at `/usr/share/X11/xkb/keycodes/` which associates numeric keycodes (key number on keyboard) to "key names".

RemapKeys
=========

From `man Xvnc`:

    −RemapKeys mapping
      Sets up a keyboard mapping. mapping is a comma-separated string of character mappings, 
      each of the form 
      char->char, or char<>char, 
      where char is a hexadecimal keysym. 
      For example, to exchange the " and @ symbols you would specify the following:
      RemapKeys=0x22<>0x40

This means that in `~/.vnc/config` I should add line:

    RemapKeys=...

