#!/bin/sh

# Ensure XServer is fully initialized before applying xmodmap

# Disable IBM TrackPoint
(xinput disable "TPPS/2 IBM TrackPoint") &

# Enable Synaptics palm detection
(xinput set-prop "Synaptics TM3276-022" "Synaptics Palm Detection" 1)         &
(xinput set-prop "Synaptics TM3276-022" "Synaptics Tap Action" 1 1 1 1 1 1 1) &
(xinput set-prop "Synaptics TM3276-022" "Synaptics Tap Time" 100)             &

# Disable left-click (button 1) on device ID 10
# This disables left-click, while keeping right-click, middle-click, scroll, etc.
# Device ID 10 is equal to 'Synaptics TM3276-022'
(xinput set-button-map 10 0 2 3 4 5 6 7 8 9 10 11 12) &

# Disable touchpad while typing (syndaemon)
(syndaemon -i 1.0 -K -t -d) &

# Start dwmblock
(dwmblocks) &

# Start emacs pinky finger
(sleep 1 && xmodmap ~/.local/bin/emacs_keyboard) &

# Start window manager
(exec /usr/local/bin/dwm)
