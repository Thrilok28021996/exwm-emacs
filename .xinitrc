#!/bin/sh
# .xinitrc for EXWM on Guix System

# Set environment variables
export XDG_CURRENT_DESKTOP=EXWM
export XDG_SESSION_TYPE=x11
export XDG_SESSION_DESKTOP=EXWM

# Start essential services
pulseaudio --start &
nm-applet &
dunst &
redshift -l 40.7:-74.0 &

# Set wallpaper
xsetroot -solid "#2F3349"

# Configure input devices
xset r rate 300 50
setxkbmap us

# Start EXWM
exec emacs -l ~/.emacs.d/init.el