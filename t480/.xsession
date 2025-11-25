# ============================================
# Author               : Agung Tuanany
# Last Modified        : Wed Nov 26 06:25:00 AM WIB 2025
# ============================================

#!/bin/sh

# Wait until X devices are fully available
sleep 1

# Disable TrackPoint (if still detected)
if xinput list | grep -q "TPPS/2 IBM TrackPoint"; then
    xinput disable "TPPS/2 IBM TrackPoint"
fi

# Disable G2Touch (floating but still appears)
if xinput list | grep -q "G2Touch"; then
    xinput disable "G2Touch Multi-Touch by G2TSP"
fi

# Touchpad tuning using libinput (synaptics replaced)
if xinput list | grep -q "Synaptics TM3276-022"; then
    xinput set-prop "Synaptics TM3276-022" "libinput Tapping Enabled" 1
    xinput set-prop "Synaptics TM3276-022" "libinput Disable While Typing Enabled" 1
fi

# Disable touchpad while typing (extra layer)
(syndaemon -i 1.0 -K -t -d) &

# Start dwmblocks (if installed)
(dwmblocks >/dev/null 2>&1) &

# Apply Emacs keyboard Pinky redirection
(sleep 1 && xmodmap ~/.local/bin/emacs_keyboard) &

# Start Window Manager
exec /usr/local/bin/dwm
