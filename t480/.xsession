#!/bin/sh

# Ensure X server is fully initialized before applying xmodmap
(sleep 1 && xmodmap ~/.local/bin/emacs_keyboard) &

# Start window manager
exec /usr/local/bin/dwm
