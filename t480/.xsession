#!/bin/sh

# Ensure XServer is fully initialized before applying xmodmap

# Start dwmblock
(dwmblocks &)

# reduce Synaptics Tap Time
(xinput set-prop "Synaptics TM3276-022" "Synaptics Tap Time" 100) &

# start emacs pinky finger
(sleep 1 && xmodmap ~/.local/bin/emacs_keyboard) &

# Start window manager
(exec /usr/local/bin/dwm)
