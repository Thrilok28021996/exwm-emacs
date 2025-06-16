;; Guix System Configuration for EXWM Desktop Environment
;; This configuration provides a complete desktop environment using EXWM as the window manager

(use-modules (gnu)
             (gnu system)
             (gnu system nss)
             (gnu services)
             (gnu services base)
             (gnu services desktop)
             (gnu services networking)
             (gnu services xorg)
             (gnu services sound)
             (gnu services sddm)
             (gnu services syncthing)
             (gnu services virtualization)
             (gnu services mcron)
             (gnu packages)
             (gnu packages admin)
             (gnu packages audio)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages cups)
             (gnu packages desktop)
             (gnu packages disk)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages freedesktop)
             (gnu packages file)
             (gnu packages game-development)
             (gnu packages games)
             (gnu packages gimp)
             (gnu packages gnome)
             (gnu packages gnupg)
             (gnu packages graphics)
             (gnu packages gstreamer)
             (gnu packages gtk)
             (gnu packages hunspell)
             (gnu packages image)
             (gnu packages image-viewers)
             (gnu packages libreoffice)
             (gnu packages linux)
             (gnu packages mail)
             (gnu packages music)
             (gnu packages password-utils)
             (gnu packages pdf)
             (gnu packages photo)
             (gnu packages pulseaudio)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages rust-apps)
             (gnu packages screen)
             (gnu packages ssh)
             (gnu packages syncthing)
             (gnu packages terminals)
             (gnu packages text-editors)
             (gnu packages video)
             (gnu packages virtualization)
             (gnu packages wm)
             (gnu packages web-browsers)
             (gnu packages xdisorg)
             (gnu packages xorg)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(operating-system
  ;; Use non-free Linux kernel for better hardware support
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  
  (locale "en_US.utf8")
  (timezone "America/New_York") ; Adjust to your timezone
  (keyboard-layout (keyboard-layout "us"))
  
  (host-name "exwm-desktop")
  
  ;; User account
  (users (cons* (user-account
                 (name "user") ; Change this to your preferred username
                 (comment "EXWM User")
                 (group "users")
                 (home-directory "/home/user")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "lp" "cdrom" "kvm" "libvirt")))
                %base-user-accounts))

  ;; Packages installed system-wide
  (packages
   (append
    (list
     ;; === Core Desktop Environment ===
     emacs
     emacs-exwm
     emacs-desktop-environment
     
     ;; === Web Browsers ===
     firefox
     chromium
     
     ;; === Terminal Emulators ===
     alacritty
     foot
     gnome-terminal
     
     ;; === File Managers ===
     thunar
     pcmanfm
     ranger
     
     ;; === Text Editors ===
     vim
     neovim
     
     ;; === Development Tools ===
     git
     git-lfs
     make
     gcc-toolchain
     python
     python-pip
     node
     
     ;; === System Utilities ===
     htop
     btop
     tree
     unzip
     zip
     curl
     wget
     rsync
     
     ;; === Audio/Video ===
     pulseaudio
     pavucontrol
     alsa-utils
     mpv
     vlc
     ffmpeg
     
     ;; === Graphics and Image ===
     gimp
     inkscape
     feh
     imagemagick
     
     ;; === Office Suite ===
     libreoffice
     
     ;; === PDF Viewers ===
     zathura
     evince
     
     ;; === Network Tools ===
     network-manager
     network-manager-applet
     openssh
     
     ;; === Fonts ===
     font-fira-code
     font-awesome
     font-dejavu
     font-liberation
     font-gnu-freefont
     font-google-noto
     font-jetbrains-mono
     
     ;; === X11 Utilities ===
     xorg-server
     xrandr
     xset
     xsetroot
     xprop
     xwininfo
     xdotool
     scrot
     maim
     xclip
     xsel
     
     ;; === Window Manager Utilities ===
     rofi
     dmenu
     slock
     dunst
     
     ;; === System Monitoring ===
     neofetch
     lm-sensors
     
     ;; === Archive Tools ===
     p7zip
     unrar
     
     ;; === Password Management ===
     password-store
     
     ;; === Screen Brightness ===
     brightnessctl
     
     ;; === Power Management ===
     tlp
     powertop
     
     ;; === Virtualization ===
     qemu
     virt-manager
     
     ;; === Additional Utilities ===
     syncthing
     redshift
     bluez
     bluez-alsa
     )
    %base-packages))

  ;; System services
  (services
   (append
    (list
     ;; === Display Manager ===
     ;; Using mingetty with auto-login for seamless EXWM startup
     (service mingetty-service-type
              (mingetty-configuration
               (tty "tty1")
               (auto-login "user"))) ; Change "user" to your username
     
     ;; === X11 Service ===
     (service xorg-server-service-type
              (xorg-configuration
               (keyboard-layout keyboard-layout)
               (extra-config
                (list "Section \"InputClass\"
    Identifier \"Touchpad\"
    MatchIsTouchpad \"on\"
    Driver \"libinput\"
    Option \"Tapping\" \"on\"
    Option \"NaturalScrolling\" \"true\"
    Option \"ClickMethod\" \"clickfinger\"
EndSection"))))
     
     ;; === Network Services ===
     (service network-manager-service-type)
     (service wpa-supplicant-service-type)
     
     ;; === Audio Services ===
     (service pulseaudio-service-type)
     
     ;; === Bluetooth ===
     (service bluetooth-service-type)
     
     ;; === Power Management ===
     (service tlp-service-type)
     
     ;; === Printing ===
     (service cups-service-type)
     
     ;; === NTP Time Sync ===
     (service ntp-service-type)
     
     ;; === SSH ===
     (service openssh-service-type
              (openssh-configuration
               (openssh openssh-sans-x)
               (port-number 22)))
     
     ;; === Syncthing ===
     (service syncthing-service-type
              (syncthing-configuration (user "user"))) ; Change to your username
     
     ;; === Virtualization ===
     (service libvirt-service-type)
     (service virtlog-service-type)
     
     ;; === Cron Jobs ===
     (service mcron-service-type)
     
     ;; === Desktop Services ===
     (service elogind-service-type)
     (service dbus-service-type)
     (service polkit-service-type)
     (service fontconfig-file-system-service)
     
     ;; === Base Services ===
     (service guix-service-type)
     (service special-files-service-type
              `(("/usr/bin/env" ,(file-append coreutils "/bin/env"))
                ("/bin/sh" ,(file-append bash "/bin/sh"))))
     )
    
    ;; Remove gdm and other desktop services we don't need
    (remove (lambda (service)
              (or (eq? (service-kind service) gdm-service-type)
                  (eq? (service-kind service) desktop-services)))
            %base-services)))

  ;; Bootloader configuration
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets (list "/boot/efi"))
    (keyboard-layout keyboard-layout)))

  ;; File systems
  ;; NOTE: Adjust these mount points according to your actual partition setup
  (file-systems
   (cons* (file-system
           (mount-point "/boot/efi")
           (device (uuid "1234-ABCD"
                         'fat32)) ; Replace with your actual EFI partition UUID
           (type "vfat"))
          (file-system
           (mount-point "/")
           (device (uuid "your-root-uuid"
                         'ext4)) ; Replace with your actual root partition UUID
           (type "ext4"))
          %base-file-systems))

  ;; Swap devices
  (swap-devices
   (list (swap-space
          (target (uuid "your-swap-uuid"))))) ; Replace with your actual swap UUID

  ;; Name service switch
  (name-service-switch %mdns-host-lookup-nss))