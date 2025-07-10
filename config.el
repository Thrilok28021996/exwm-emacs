;; EXWM Configuration for Doom Emacs
;; Add this to your Doom Emacs config.el

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
(setq doom-font (font-spec :family "Fira Code" :size 15))

;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Set the C/C++ style. "file" will look for a .clang-format file in your
;; project's directory. You can also use "llvm", "google", "chromium", etc.
;; Configure C/C++ indentation and style
(after! cc-mode
  ;; Set the fundamental indentation to 4 spaces
  (setq c-basic-offset 4)

  ;; Set the visual width of tab characters to 4 spaces
  ;; This doesn't mean "use tabs", it just defines their width if they appear.
  (setq-default tab-width 4)

  ;; IMPORTANT: Tell Emacs to use spaces for indentation, not actual tab characters.
  ;; This is the standard for most modern projects.
  (setq indent-tabs-mode nil)

  ;; If you want to define a custom style directly in your config instead of
  ;; relying on file-local variables, you can do this:
  ;; (c-add-style "my-style"
  ;;              '("gnu" ; Start with the "gnu" style as a base
  ;;                (c-basic-offset . 4)
  ;;                (indent-tabs-mode . nil)))
  ;; And then set it as the default:
  ;; (setq c-default-style "my-style")
  )

;; --- Python & Conda Configuration ---
;; The +conda flag in init.el already loads conda.el for you.
;; This makes the `conda-env-activate` command available.

;; When you activate a conda environment, lsp-mode (and pyright) will
;; automatically use the Python interpreter and packages from that env.
;; This is the "magic" that makes it all work seamlessly.

;; Optional: Point to your preferred python interpreter for general use.
;; When a conda env is active, it will override this.
;; (setq python-shell-interpreter "python3")


;; --- C++ Configuration ---
;; For clangd (LSP) to work correctly, it needs a `compile_commands.json` file.
;; This file tells clangd how your project is compiled.
;;
;; You can generate it easily with CMake:
;;
;;   $ cd /path/to/your/project
;;   $ mkdir build && cd build
;;   $ cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
;;
;; `clangd` will automatically find this file when you open a C++ file in
;; that project. There's usually no extra Emacs configuration needed for this.


;; --- Formatter Configuration ---
;; This ensures that `black` is used for Python and `clang-format` for C++.
;; The :editor (format +onsave) in init.el will trigger these on save.


;; This block configures Python mode.
(after! 'python
  ;; 1. Set the FORMATTER to `ruff format`.
  ;;    This is triggered on save by the `+onsave` flag.
  (setq-hook! 'python-mode-hook +format-with 'ruff-format)

  ;; 2. Set the LINTER to `ruff`.
  ;;    This replaces flake8 for on-the-fly error checking.
  (setq-hook! 'python-mode-hook flycheck-checker 'python-ruff))




(after! 'cc
  ;; lsp-format-buffer will use the LSP server's formatter, which is clangd.
  ;; clangd, in turn, uses clang-format.
  (setq-hook! 'c-mode-hook +format-with 'lsp)
  (setq-hook! 'c++-mode-hook +format-with 'lsp))


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
