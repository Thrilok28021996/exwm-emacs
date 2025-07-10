;; EXWM Desktop Environment Configuration for Guix
;; This configuration sets up EXWM with essential desktop services

(use-modules (gnu)
             (gnu system)
             (gnu services)
             (gnu services desktop)
             (gnu services xorg)
             (gnu services networking)
             (gnu services audio)
             (gnu services databases)
             (gnu services mcron)
             (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages wm)
             (gnu packages xorg)
             (gnu packages fonts)
             (gnu packages audio)
             (gnu packages pulseaudio)
             (gnu packages linux)
             (gnu packages admin)
             (gnu packages terminals)
             (gnu packages web-browsers)
             (gnu packages image-viewers)
             (gnu packages pdf)
             (gnu packages file-manager)
             (gnu packages gnome)
             (gnu packages freedesktop))

(define exwm-desktop-service-type
  (service-type
   (name 'exwm-desktop)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           (lambda (config)
             (list (shepherd-service
                    (provision '(exwm-desktop))
                    (requirement '(user-processes host-name))
                    (start #~(make-forkexec-constructor
                              (list #$(file-append emacs "/bin/emacs")
                                    "--eval" "(require 'exwm)"
                                    "--eval" "(exwm-enable)")
                              #:environment-variables
                              '("DISPLAY=:0")))
                    (stop #~(make-kill-destructor))))))
          (service-extension
           xorg-configuration-service-type
           (lambda (config)
             (xorg-configuration
              (inherit config)
              (server (xorg-server))
              (keyboard-layout (keyboard-layout "us"))
              (extra-config
               (list "Section \"ServerFlags\"
    Option \"DontVTSwitch\" \"true\"
    Option \"DontZap\" \"false\"
EndSection")))))))
   (default-value #f)
   (description "EXWM desktop environment service.")))

(operating-system
  (locale "en_US.utf8")
  (timezone "America/New_York")  ; Change to your timezone
  (keyboard-layout (keyboard-layout "us"))
  (host-name "exwm-desktop")
  (users (cons* (user-account
                 (name "user")  ; Change to your username
                 (comment "EXWM User")
                 (group "users")
                 (home-directory "/home/user")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  (packages
   (append
    (specifications->packages
     '(;; Core EXWM packages
       "emacs"
       "emacs-exwm"
       "emacs-desktop-environment"
       
       ;; Doom Emacs dependencies
       "git"
       "ripgrep"
       "fd"
       "emacs-vterm"
       
       ;; Essential desktop utilities
       "dmenu"
       "scrot"
       "xorg-server"
       "xrandr"
       "xset"
       "xsetroot"
       "xprop"
       "xwininfo"
       "xkill"
       
       ;; Audio
       "pulseaudio"
       "pavucontrol"
       "alsa-utils"
       
       ;; Network
       "network-manager"
       "network-manager-applet"
       
       ;; Fonts
       "font-fira-code"
       "font-awesome"
       "font-dejavu"
       "font-liberation"
       
       ;; Applications
       "firefox"
       "alacritty"
       "thunar"
       "feh"
       "mpv"
       "evince"
       
       ;; System utilities
       "htop"
       "neofetch"
       "tree"
       "unzip"
       "zip"))
    %base-packages))

  (services
   (append
    (list
     ;; Basic desktop services
     (service network-manager-service-type)
     (service wpa-supplicant-service-type)
     (service bluetooth-service-type)
     (service cups-service-type)
     
     ;; Audio services
     (service pulseaudio-service-type)
     (service alsa-service-type)
     
     ;; Display manager (minimal)
     (service slim-service-type
              (slim-configuration
               (display ":0")
               (vt "vt7")
               (auto-login? #t)
               (default-user "user")  ; Change to your username
               (xorg-configuration
                (xorg-configuration
                 (keyboard-layout (keyboard-layout "us"))
                 (extra-config
                  (list "Section \"ServerFlags\"
    Option \"DontVTSwitch\" \"true\"
    Option \"DontZap\" \"false\"
EndSection"))))))
     
     ;; EXWM service
     (simple-service 'exwm-config
                     home-files-service-type
                     `((".xinitrc"
                        ,(plain-file "xinitrc"
                                     "#!/bin/sh
# Set up environment for EXWM
export EDITOR=emacs
export BROWSER=firefox
export TERMINAL=alacritty

# Start EXWM
exec emacs --eval \"(progn (require 'exwm) (exwm-enable))\"
"))))
     
     ;; Polkit for privilege escalation
     (polkit-service-type))
    
    ;; Base services with modifications
    (modify-services %base-services
      (guix-service-type
       config =>
       (guix-configuration
        (inherit config)
        (substitute-urls
         (append (list "https://ci.guix.gnu.org"
                       "https://bordeaux.guix.gnu.org")
                 %default-substitute-urls))
        (authorized-keys
         (append (list (local-file "./signing-key.pub" "signing-key.pub"))
                 %default-authorized-guix-keys)))))))

  (bootloader
   (bootloader-configuration
    (bootloader grub-bootloader)
    (targets (list "/dev/vda"))  ; Change to your boot device
    (keyboard-layout keyboard-layout)))

  (swap-devices
   (list (swap-space
          (target (uuid "db67426c-db92-4baf-be35-2781fee7a5a5")))))  ; Change to your swap UUID

  (file-systems
   (cons* (file-system
           (mount-point "/")
           (device (uuid "421ed21a-ff9e-404f-b904-58345bbf4b06"))  ; Change to your root UUID
           (type "ext4"))
          %base-file-systems)))
