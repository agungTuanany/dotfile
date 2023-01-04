#!/bin/sh

##
# ABOUT:
# use this file to ensure that 'dwm' is run with your Display Manager.
#
# RECOMMEND:
#
# - Use with symbolic links:
# if you want to use symbolic links, make sure to create them first:
# ln -sf $HOME/Repo/agung_dotfile/.xinitrc $HOME/.xinitrc
#
# - Plan:
# or simply copy them into your home directory.
#
# REPO: https://github.com/agungTuanany/dotfile.git
##

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# capslock key
#xmodmap ~/.local/bin/emacs_keyboard &

#Compositor

# Wallpeper
# nitrogen --restore &

# Loop
while true; do
    dwm > dev/null 2>&1
done

# Execute DWM
exec dwm
