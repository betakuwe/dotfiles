;; Set colour theme
(setq catppuccin-flavor 'latte)
(setq prelude-theme 'catppuccin)

;; Don't use super keybindings
(setq prelude-super-keybindings nil)

;; Font type
;; [WARN] Won't work if system cannot render this font
(set-face-attribute 'default nil :font "Maple Mono")

;; Set text height
(defun set-height-by-system-name-and-display ()
  (thread-last
    (cond
     ;; On macbook air
     ((equal system-type 'darwin) 150)
     ;; Else on work laptop
     ((if (> (x-display-pixel-width) 2560) 75 100)))
    (set-face-attribute 'default nil :height)))

(set-height-by-system-name-and-display)

;; Supposedly this should call the height setting function when the display monitor changes.
;; [TODO] Test this, I don't know if this works.
(setq display-monitors-changed-functions #'set-height-by-system-name-and-display)

;; disable scrollbars in emacs
(scroll-bar-mode -1)

;; flex adds fuzzy finding
(add-to-list 'completion-styles 'flex)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Softwrap and still kill to end of line instead of killing visual line only
;; maybe consider disable the go to beginning or end of visual line too?
(global-visual-line-mode 1)
(keymap-unset visual-line-mode-map "C-k")

;; Display startup time on emacs first launch
(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 100)

;; Dired buffers: Automatically hide file details (permissions, size,
;; modification date, etc.) and all the files in the `dired-omit-files' regular
;; expression for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Splits horizontally (left-right) when this value is below window width
(setq split-width-threshold 80)

(keymap-global-set "C-c R" 'recentf)
(keymap-global-set "C-c O" 'pop-global-mark)
(keymap-global-set "C-c C" 'compile)

;; Do I need this?
(setenv "ANDROID_HOME" (concat (getenv "HOME") "/Android/Sdk"))
