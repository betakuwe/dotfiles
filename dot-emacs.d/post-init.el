;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))

(use-package emacs
  :ensure nil
  :hook
  (after-init . (lambda ()
                  (global-auto-revert-mode)
                  (let ((inhibit-message t))
                    (recentf-mode 1))
                  (save-place-mode)
                  (display-time-mode)
                  (show-paren-mode)
                  (winner-mode)
                  (delete-selection-mode)
                  (window-divider-mode)
                  (repeat-mode)
                  (electric-pair-mode)
                  (minibuffer-depth-indicate-mode)
                  (recentf-cleanup)))
  (compilation-filter . ansi-color-compilation-filter)
  (emacs-startup . display-startup-time)

  :custom
  (show-trailing-whitespace t)
  (split-width-threshold 100)
  (enable-recursive-minibuffers t)
  (custom-file (make-temp-file "emacs-custom"))
  (display-time-24hr-format t)
  (redisplay-skip-fontification-on-input t)
  (compile-command "")
  (compilation-scroll-output t)
  (display-line-numbers-type 'relative)
  (mode-require-final-newline 'ask)
  (custom-unlispify-tag-names nil)
  ;; Use treesitter modes instead
  ;; (font-lock-support-mode 'tree-sitter-lock-mode)
  ;; (major-mode-remap-alist '((bash-mode . bash-ts-mode)
  ;;                           (c-mode . c-ts-mode)
  ;;                           (c++-mode . c++-ts-mode)
  ;;                           (c-or-c++-mode . c-or-c++-ts-mode)
  ;;                           (python-mode . python-ts-mode)))

  :config
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

  (global-display-line-numbers-mode 1)

  ;; Softwrap and still kill to end of line instead of killing visual line only
  ;; maybe consider disable the go to beginning or end of visual line too?
  (global-visual-line-mode 1)
  (keymap-unset visual-line-mode-map "C-k")

  ;; Make lambda appear as a symbol
  (global-prettify-symbols-mode 1))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ;; Go up directory and reuse buffer (don't create new buffers)
              ("^" . (lambda ()
                       (interactive)
                       (find-alternate-file ".."))))
  :custom
  ;; (dired-kill-when-opening-new-dired-buffer nil)
  (dired-recursive-deletes 'always)
  (dired-hide-details-hide-symlink-targets nil))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

;; (use-package viper
;;   :ensure nil
;;   :demand t
;;   ;; The only way I know to make async shell command start in vi mode, not sure if there's a better way than this
;;   :hook (shell-command-mode . viper-intercept-ESC-key)
;;   :custom
;;   (viper-inhibit-startup-message 't)
;;   (viper-expert-level '5)
;;   (viper-emacs-state-mode-list nil)
;;   (viper-insert-state-mode-list nil)
;; 
;;   ;; Rudimentary changes
;;   (viper-auto-indent 't)
;;   (viper-case-fold-search 't)
;;   (viper-re-search 't)
;;   (viper-shift-width 2)
;;   (viper-re-query-replace 't)
;;   (viper-want-ctl-h-help 't)
;;   ;; (viper-no-multiple-ESC nil) ;ESC to activate M- (don't really like it tbh)
;;   (viper-ex-style-motion nil)
;;   (viper-ex-style-editing nil)
;;   (viper-custom-file-name "~/.emacs.d/post-init.el")
;; 
;;   (viper-mode 't)
;;   :bind (:map viper-vi-basic-map
;;               ("K" . #'eldoc)
;;               ("C-o" . #'pop-global-mark)
;;               ("<backspace>" . #'dired-jump)
;;               ("d" . (lambda ()
;;                        (interactive)
;;                        (if mark-active
;;                            (kill-region (region-beginning)
;;                                         (1+ (region-end)))
;;                          (viper-command-argument nil)))))
;;   :config
;;   (viper-modify-major-mode 'dired-mode 'vi-state
;;                            (define-keymap :parent viper-vi-basic-map
;;                              "<return>" #'dired-find-file
;;                              "S-<return>" #'dired-maybe-insert-subdir
;;                              "<backspace>" #'dired-up-directory
;;                              "i" #'wdired-change-to-wdired-mode
;;                              "d" #'dired-flag-file-deletion
;;                              "D" #'dired-kill-subdir
;;                              "x" #'dired-do-flagged-delete
;;                              "r" #'revert-buffer
;;                              "(" #'dired-hide-details-mode))
;;   (viper-modify-major-mode 'help-mode 'vi-state
;;                            (define-keymap :parent viper-vi-basic-map
;;                              "q" #'quit-window
;;                              "i" #'help-goto-info
;;                              "s" #'help-view-source
;;                              "<backspace>" #'help-go-back
;;                              "S-<backspace>" #'help-go-forward
;;                              "r" #'revert-buffer))
;;   (viper-modify-major-mode 'special-mode 'vi-state
;;                            (define-keymap :parent viper-vi-basic-map
;;                              "q" #'quit-window
;;                              "r" #'revert-buffer))
;;   (viper-modify-major-mode 'Info-mode 'vi-state
;;                            (define-keymap :parent viper-vi-basic-map
;;                              "S-<backspace>" #'Info-up
;;                              "<return>" #'Info-follow-nearest-node))
;;   (viper-modify-major-mode 'compilation-mode 'vi-state
;;                            (define-keymap :parent viper-vi-basic-map
;;                              "r" #'recompile))
;;   (cl-callf append viper-vi-state-mode-list
;;     '(help-mode
;;       dired-mode
;;       compilation-mode
;;       completion-list-mode
;;       Custom-mode
;;       debugger-mode
;;       special-mode
;;       shell-command-mode
;;       shell-mode))
;;   (viper-buffer-search-enable)
;;   (viper-mode))

(use-package ido
  :ensure nil
  :custom
  (ido-enable-flex-matching 't)
  (ido-everywhere 't)
  (ido-use-filename-at-point 'guess)
  :config
  (ido-mode 1))

(use-package icomplete
  :ensure nil
  :config
  ;; (icomplete-mode 1)
  (fido-mode 1))

(use-package completion
  :ensure nil
  :demand t
  :bind
  (:map completion-in-region-mode-map
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion)
        ("C-y" . minibuffer-choose-completion))
  (:map completion-in-region-mode-map
        ("C-n" . minibuffer-next-completion)
        ("C-p" . minibuffer-previous-completion)
        ("C-y" . minibuffer-choose-completion))
  :custom
  (completions-max-height 20)
  (completion-auto-select nil)
  :config
  (cl-callf append completion-styles '(substring initials flex)))

(use-package tab-bar
  :ensure nil
  :custom (tab-bar-show 1)
  :config (tab-bar-mode))

;; Add to path env, workstation specific
;; TODO write a function to automate this
(let ((exec-path-file "./exec-path.el"))
  (when (file-exists-p exec-path-file)
    (load-file exec-path-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set colour theme
(use-package catppuccin-theme
  :custom (catppuccin-flavor 'latte)
  :config (load-theme 'catppuccin :no-confirm))

;; fancypants
(use-package ligature
  :config
  ;; Enable Maple Mono ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
     ;; =:= =!=
     ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
     ;; ;; ;;;
     (";" (rx (+ ";")))
     ;; && &&&
     ("&" (rx (+ "&")))
     ;; !! !!! !. !: !!. != !== !~
     ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
     ;; ?? ??? ?:  ?=  ?.
     ("?" (rx (or ":" "=" "\." (+ "?"))))
     ;; %% %%%
     ("%" (rx (+ "%")))
     ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
     ;; |->>-||-<<-| |- |== ||=||
     ;; |==>>==<<==<=>==//==/=!==:===>
     ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                     "-" "=" ))))
     ;; \\ \\\ \/
     ("\\" (rx (or "/" (+ "\\"))))
     ;; ++ +++ ++++ +>
     ("+" (rx (or ">" (+ "+"))))
     ;; :: ::: :::: :> :< := :// ::=
     (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
     ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
     ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                     "="))))
     ;; .. ... .... .= .- .? ..= ..<
     ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
     ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
     ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
     ;; *> */
     ("*" (rx (or ">" "/")))
     ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
     ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
     ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
     ;; << <<< <<<<
     ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                     "-"  "/" "|" "="))))
     ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
     ;; >> >>> >>>>
     (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
     ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
     ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                  (+ "#"))))
     ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
     ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
     ;; __ ___ ____ _|_ __|____|_
     ("_" (rx (+ (or "_" "|"))))
     ;; Fira code: 0xFF 0x12
     ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
     ;; Fira code:
     ;; "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
     ;; The few not covered by the regexps.
     "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="
     "[TODO]" "[DEBUG]" "[INFO]" "[WARN]" "[ERROR]" "[FIXME]"))
  (global-ligature-mode t))

;; (use-package meow
;;   :config
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     (meow-motion-overwrite-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev)
;;      '("<escape>" . ignore))
;;     (meow-leader-define-key
;;      ;; Use SPC (0-9) for digit arguments.
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("A" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("h" . meow-left)
;;      '("H" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("I" . meow-open-above)
;;      '("j" . meow-next)
;;      '("J" . meow-next-expand)
;;      '("k" . meow-prev)
;;      '("K" . meow-prev-expand)
;;      '("l" . meow-right)
;;      '("L" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("o" . meow-block)
;;      '("O" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . ignore)))
;;   (require 'meow)
;;   (meow-setup)
;;   (meow-global-mode 1))

;; (use-package meow-tree-sitter
;;   :config
;;   (meow-tree-sitter-register-defaults))

(use-package zoom
  :custom
  (zoom-mode t)
  (zoom-size '(0.618 . 0.618)))

;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; (use-package tree-sitter
;;   :config
;;   (require 'tree-sitter)
;;   (require 'tree-sitter-hl)
;;   (tree-sitter-hl-mode)
;;   (require 'tree-sitter-debug)
;;   (require 'tree-sitter-query)
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs
;;   :config (require 'tree-sitter-langs))

;; (use-package tree-sitter-langs)

(use-package magit)

(use-package markdown-mode :defer t)
(use-package toml-mode :defer t)
(use-package cmake-mode :defer t)

