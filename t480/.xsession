# ============================================
# Author               : Agung Tuanany
# Last Modified        : Sun Jun  7 05:44:16 AM WIB 2026
# ============================================

#!/bin/sh

[ -f "$HOME/.xprofile" ] && . "$HOME/.xprofile"

dwmblocks &
exec /usr/local/bin/dwm
