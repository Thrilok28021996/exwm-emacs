#!/bin/bash
# EXWM Startup Script for Guix System
# Place this script in your user's home directory and make it executable

# Set up environment variables
export XDG_CURRENT_DESKTOP=EXWM
export XDG_SESSION_TYPE=x11
export XDG_SESSION_DESKTOP=EXWM

# Set up display
export DISPLAY=:0

# Make sure user directories exist
mkdir -p ~/.config
mkdir -p ~/.local/share
mkdir -p ~/Pictures/Screenshots

# Start essential services
echo "Starting essential desktop services..."

# Start PulseAudio if not running
if ! pgrep -x "pulseaudio" > /dev/null; then
    pulseaudio --start --log-target=syslog &
fi

# Start network manager applet
if command -v nm-applet >/dev/null 2>&1; then
    nm-applet &
fi

# Start Bluetooth applet if available
if command -v blueman-applet >/dev/null 2>&1; then
    blueman-applet &
fi

# Start volume control tray icon
if command -v pasystray >/dev/null 2>&1; then
    pasystray &
fi

# Start notification daemon
if command -v dunst >/dev/null 2>&1; then
    dunst &
fi

# Start screen brightness control
if command -v brightnessctl >/dev/null 2>&1; then
    # Set default brightness to 50%
    brightnessctl set 50%
fi

# Start redshift for eye protection
if command -v redshift >/dev/null 2>&1; then
    redshift -l 40.7:-74.0 &  # Adjust coordinates to your location
fi

# Set wallpaper if feh is available
if command -v feh >/dev/null 2>&1; then
    # Create a default wallpaper directory
    mkdir -p ~/Pictures/Wallpapers
    
    # If wallpaper exists, set it
    if [ -f ~/Pictures/Wallpapers/wallpaper.jpg ]; then
        feh --bg-scale ~/Pictures/Wallpapers/wallpaper.jpg
    else
        # Set a solid color background
        xsetroot -solid "#2F3349"
    fi
fi

# Configure X11 settings
if command -v xset >/dev/null 2>&1; then
    # Set key repeat rate
    xset r rate 300 50
    
    # Disable screen saver blanking
    xset s off
    xset -dpms
fi

# Set up keyboard layout if needed
if command -v setxkbmap >/dev/null 2>&1; then
    setxkbmap us  # Change to your preferred layout
fi

# Configure mouse/touchpad if needed
if command -v xinput >/dev/null 2>&1; then
    # Enable tap to click for touchpads
    for id in $(xinput list | grep -i touchpad | grep -o 'id=[0-9]*' | cut -d= -f2); do
        xinput set-prop $id "libinput Tapping Enabled" 1 2>/dev/null || true
        xinput set-prop $id "libinput Natural Scrolling Enabled" 1 2>/dev/null || true
    done
fi

# Start Emacs with EXWM
echo "Starting Emacs with EXWM..."

# Ensure Emacs uses the correct configuration
export EMACS_CONFIG_DIR=~/.emacs.d

# Start Emacs with EXWM
exec emacs --eval "(progn (require 'exwm) (exwm-enable))"