#!/bin/bash
# EXWM Startup Script
# Save this as ~/.local/bin/start-exwm and make it executable

# Set environment variables
export EDITOR="emacs"
export BROWSER="firefox"
export TERMINAL="alacritty"

# Set up X11 display
export DISPLAY=:0

# Configure audio
pulseaudio --start --log-target=syslog &

# Set wallpaper (optional)
if command -v feh &> /dev/null; then
    feh --bg-scale ~/Pictures/wallpaper.jpg &
fi

# Start compositor for transparency effects (optional)
if command -v picom &> /dev/null; then
    picom -b &
fi

# Start network manager applet
nm-applet &

# Start bluetooth applet
blueman-applet &

# Start audio system tray
pasystray &

# Set up keyboard layout
setxkbmap us

# Disable screen saver and power management
xset s off
xset -dpms

# Start EXWM with Doom Emacs
exec emacs --eval "(progn (require 'exwm) (exwm-enable))"
