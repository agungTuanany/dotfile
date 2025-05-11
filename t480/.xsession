#!/bin/sh

# Ensure XServer is fully initialized before applying xmodmap

# Disable IBM TrackPoint
(xinput disable "TPPS/2 IBM TrackPoint") &

# Enable Synaptics palm detection
(xinput set-prop "Synaptics TM3276-022" "Synaptics Palm Detection" 1)         &
(xinput set-prop "Synaptics TM3276-022" "Synaptics Tap Action" 1 1 1 1 1 1 1) &
(xinput set-prop "Synaptics TM3276-022" "Synaptics Tap Time" 100)             &

# Disable touchpad while typing (syndaemon)
(syndaemon -i 1.0 -K -t -d) &

# Start dwmblock
(dwmblocks &)

# start emacs pinky finger
(sleep 1 && xmodmap ~/.local/bin/emacs_keyboard) &

# Start window manager
(exec /usr/local/bin/dwm)
