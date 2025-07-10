;; EXWM Configuration for Doom Emacs
;; Add this to your Doom Emacs config.el

(use-package exwm
  :config
  ;; Number of workspaces
  (setq exwm-workspace-number 4)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Global keybindings
  (setq exwm-input-global-keys
        `(
          ;; Bind "s-r" to exit char-mode and fullscreen mode
          ([?\s-r] . exwm-reset)
          
          ;; Bind "s-w" to switch workspace interactively
          ([?\s-w] . exwm-workspace-switch)
          
          ;; Bind "s-0" to "s-9" to switch to corresponding workspace
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          
          ;; Bind "s-&" to launch applications ('counsel-linux-app' if available)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          
          ;; Bind "s-<f2>" to "slock" screen locker
          ([s-f2] . (lambda ()
                      (interactive)
                      (start-process "" nil "/usr/bin/slock")))
          
          ;; Application shortcuts
          ([?\s-t] . (lambda ()
                       (interactive)
                       (start-process "" nil "alacritty")))
          ([?\s-f] . (lambda ()
                       (interactive)
                       (start-process "" nil "firefox")))
          ([?\s-e] . (lambda ()
                       (interactive)
                       (start-process "" nil "thunar")))
          ))

  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(
          ;; Movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; Cut/paste
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; Search
          ([?\C-s] . [?\C-f])))

  ;; Enable EXWM
  (exwm-enable))

;; Desktop Environment integration
(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode))

;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; Randr support for multiple monitors
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal")))
(exwm-randr-enable)

;; Workspace configuration
(defun exwm-workspace-switch-create (n)
  "Switch to workspace N, creating it if it doesn't exist."
  (interactive "nWorkspace: ")
  (let ((workspace-count (exwm-workspace--count)))
    (if (< n workspace-count)
        (exwm-workspace-switch n)
      (dotimes (i (- n workspace-count -1))
        (exwm-workspace-add))
      (exwm-workspace-switch n))))

;; Window management functions
(defun exwm-toggle-floating ()
  "Toggle floating state of current window."
  (interactive)
  (if exwm--floating-frame
      (exwm-floating-toggle-floating)
    (exwm-floating-toggle-floating)))

(defun exwm-move-window-to-workspace (n)
  "Move current window to workspace N."
  (interactive "nWorkspace: ")
  (exwm-workspace-move-window n))

;; Additional keybindings
(exwm-input-set-key (kbd "s-SPC") 'exwm-toggle-floating)
(exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window-to-workspace)

;; Configure specific applications
(setq exwm-manage-configurations
      '(((equal exwm-class-name "Firefox")
         char-mode t)
        ((equal exwm-class-name "Alacritty")
         char-mode t)
        ((equal exwm-class-name "mpv")
         floating t
         floating-mode-line nil)
        ((equal exwm-class-name "Pavucontrol")
         floating t)))

;; Audio control
(defun exwm-volume-up ()
  "Increase volume."
  (interactive)
  (start-process-shell-command
   "volume-up" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%"))

(defun exwm-volume-down ()
  "Decrease volume."
  (interactive)
  (start-process-shell-command
   "volume-down" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%"))

(defun exwm-volume-mute ()
  "Toggle mute."
  (interactive)
  (start-process-shell-command
   "volume-mute" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle"))

;; Bind volume keys
(exwm-input-set-key [XF86AudioRaiseVolume] 'exwm-volume-up)
(exwm-input-set-key [XF86AudioLowerVolume] 'exwm-volume-down)
(exwm-input-set-key [XF86AudioMute] 'exwm-volume-mute)

;; Brightness control (for laptops)
(defun exwm-brightness-up ()
  "Increase brightness."
  (interactive)
  (start-process-shell-command
   "brightness-up" nil "brightnessctl set +10%"))

(defun exwm-brightness-down ()
  "Decrease brightness."
  (interactive)
  (start-process-shell-command
   "brightness-down" nil "brightnessctl set 10%-"))

;; Bind brightness keys
(exwm-input-set-key [XF86MonBrightnessUp] 'exwm-brightness-up)
(exwm-input-set-key [XF86MonBrightnessDown] 'exwm-brightness-down)

;; Screenshot function
(defun exwm-screenshot ()
  "Take a screenshot."
  (interactive)
  (start-process-shell-command
   "screenshot" nil "scrot ~/Pictures/screenshot-%Y-%m-%d-%H%M%S.png"))

(exwm-input-set-key (kbd "s-s") 'exwm-screenshot)

;; Workspace naming
(defun exwm-workspace-rename ()
  "Rename current workspace."
  (interactive)
  (let ((name (read-string "Workspace name: ")))
    (exwm-workspace-rename-buffer name)))

(exwm-input-set-key (kbd "s-n") 'exwm-workspace-rename)

;; Status bar integration (if using doom-modeline)
(when (featurep 'doom-modeline)
  (setq doom-modeline-exwm-workspace t))

;; Startup applications
(defun exwm-startup-applications ()
  "Start applications on EXWM startup."
  (start-process-shell-command "nm-applet" nil "nm-applet")
  (start-process-shell-command "pasystray" nil "pasystray")
  (start-process-shell-command "blueman-applet" nil "blueman-applet"))

(add-hook 'exwm-init-hook 'exwm-startup-applications)
