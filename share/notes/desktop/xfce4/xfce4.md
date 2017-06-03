# Making default WM

To make it a default window manager, try:

    update-alternatives --config x-session-manager
    /usr/lib/i386-linux-gnu/lightdm/lightdm-set-defaults -s xfce4-session

# Configuration

## Example of using `xfconf-query`

    xfconf-query -c xfce4-keyboard-shortcuts  -p '/commands/custom/<Alt>k'

\--- this command shows what is assigned to `Alt-k`

# Themes

Notice that there are two kinds of themes: 

> &#x3d;item\* GTK theme
>
> It is set in `Aparência`. It manages everything except for window title and border.
>
> &#x3d;item\* window manager theme
>
> it is set in `Gerenciador de janelas` → `Estilo`. It manages window title and border.

# Media automounting

Sometimes I __do want__ to automount media. This is done by `Thunar`. So, I think `Thunar` should be running. 
Also, __nothing__ will happen if the flashka is not correctly formatted (for example, no filesystem). __Maybe__ I also need
to install `pmount` for this to work, but I am not sure. 

