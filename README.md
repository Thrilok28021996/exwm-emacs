# EXWM Desktop Environment for Guix System

This configuration provides a complete desktop environment using EXWM (Emacs X Window Manager) on Guix System with Firefox and essential applications.

## Quick Start

1. **Install Guix System** with the provided `system-config.scm`
2. **Copy configuration files** to your home directory
3. **Reconfigure your system** and reboot

## Files Overview

- `system-config.scm` - Complete Guix system configuration
- `init.el` - Comprehensive Emacs configuration with EXWM
- `.xinitrc` - X11 startup script
- `README.md` - This documentation

## Installation Steps

### 1. System Configuration

```bash
# Copy the system configuration
sudo cp system-config.scm /etc/config.scm

# Edit the configuration to match your hardware
sudo nano /etc/config.scm

# Important: Update these values in the config:
# - Replace "user" with your actual username
# - Update UUID values for your partitions (use `blkid` to find them)
# - Adjust timezone to your location

# Reconfigure the system
sudo guix system reconfigure /etc/config.scm
```

### 2. User Configuration

```bash
# Copy Emacs configuration
cp init.el ~/.emacs.d/init.el

# Copy X11 startup script
cp .xinitrc ~/.xinitrc
chmod +x ~/.xinitrc

# Create necessary directories
mkdir -p ~/Pictures/Wallpapers
mkdir -p ~/Pictures/Screenshots
mkdir -p ~/.local/share
```

### 3. First Boot

After rebooting, you'll be automatically logged in and EXWM will start. The system includes:

## Key Features

### Window Manager (EXWM)
- **Super+0-9**: Switch to workspace 0-9
- **Super+w**: Switch workspace interactively
- **Super+r**: Reset EXWM state
- **Super+f**: Toggle fullscreen
- **Super+q**: Close window

### Application Launchers
- **Super+b**: Firefox
- **Super+t**: Terminal (Alacritty)
- **Super+e**: File manager (Thunar)
- **Super+d**: Application launcher (Rofi)
- **Super+&**: Run command

### System Controls
- **Super+F1**: Volume controls
- **Print**: Screenshot
- **XF86MonBrightness**: Brightness control

## Included Software

### Web Browsers
- Firefox (primary)
- Chromium (backup)

### Development Tools
- Emacs with comprehensive configuration
- Git, Python, Node.js
- Multiple terminal emulators (Alacritty, Foot, GNOME Terminal)

### Multimedia
- VLC and MPV media players
- PulseAudio with pavucontrol
- GIMP and Inkscape for graphics

### Office & Productivity
- LibreOffice suite
- PDF viewers (Zathura, Evince)
- Password management (pass)

### System Utilities
- File managers (Thunar, PCManFM, Ranger)
- Network Manager with GUI
- Bluetooth support
- Power management (TLP)
- Screen brightness control

## Customization

### Changing Wallpaper
```bash
# Place your wallpaper
cp your-wallpaper.jpg ~/Pictures/Wallpapers/wallpaper.jpg

# EXWM will automatically use it on next start
```

### Adding Applications
Edit `system-config.scm` and add packages to the packages list:

```scheme
(packages
 (append
  (list
   ;; Add your packages here
   your-new-package
   another-package)
  %base-packages))
```

Then reconfigure:
```bash
sudo guix system reconfigure /etc/config.scm
```

### Emacs Configuration
The `init.el` includes:
- Evil mode (Vim keybindings)
- Modern completion (Vertico, Consult, Corfu)
- LSP support with Eglot
- Git integration with Magit
- Note-taking with Org-mode and Org-roam
- Python development environment

### Key Bindings (Space Leader)
- **Space f f**: Find file
- **Space b b**: Switch buffer
- **Space p p**: Switch project
- **Space g s**: Git status (Magit)
- **Space a b**: Launch Firefox
- **Space a t**: Launch terminal

## Troubleshooting

### EXWM Won't Start
1. Check if X11 is running: `ps aux | grep Xorg`
2. Verify Emacs configuration: `emacs --debug-init`
3. Check system logs: `journalctl -f`

### No Sound
```bash
# Restart PulseAudio
pulseaudio --kill
pulseaudio --start

# Check audio devices
pactl list sinks
```

### Network Issues
```bash
# Restart NetworkManager
sudo systemctl restart network-manager

# Check status
nmcli device status
```

### Display Issues
```bash
# Check connected displays
xrandr

# Configure dual monitors (example)
xrandr --output HDMI-1 --right-of eDP-1 --auto
```

## Advanced Configuration

### Multi-Monitor Setup
Add to your `.xinitrc` or EXWM config:
```bash
xrandr --output HDMI-1 --right-of eDP-1 --auto
```

### Custom Keybindings
Edit the `exwm-global-keys` section in `init.el` to add custom shortcuts.

### Performance Tuning
The configuration includes performance optimizations:
- Disabled backup files
- Optimized garbage collection
- Fast startup configuration

## Security Features
- Screen locker (slock) - **Super+F2**
- Password management with `pass`
- SSH server (disabled by default)
- Firewall ready (add iptables rules as needed)

## Support

For issues:
1. Check Guix manual: `info guix`
2. EXWM documentation: `C-h i` in Emacs, then select EXWM
3. Guix System troubleshooting: `/var/log/messages`

## Contributing

Feel free to customize this configuration for your needs. The setup provides a solid foundation for a productive EXWM desktop environment on Guix System.