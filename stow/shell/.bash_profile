
# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

#  # if running bash
#  if [ -n "$BASH_VERSION" ]; then
#      # include .bashrc if it exists
#      if [ -f "$HOME/.bashrc" ]; then
#  	. "$HOME/.bashrc"
#      fi
#  fi

# set PATH so it includes user's private bin if it exists
export PATH="$PATH:$HOME/bin:$HOME/usr/bin:$HOME/usr/local/bin:$HOME/.local/bin"
export MANPATH="$MANPATH:$HOME/usr/man:$HOME/usr/share/man:$HOME/usr/local/man:$HOME/usr/local/share/man"

setxkbmap -option "ctrl:nocaps"
xmodmap -e "keysym Num_Lock = Num_Lock Pointer_EnableKeys"



export PATH="$HOME/.cargo/bin:$PATH"
