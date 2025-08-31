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
  (treesit-font-lock-level 4)
  (dired-movement-style 'bounded-files)
  (confirm-kill-emacs 'y-or-n-p)
  (make-backup-files t)
  (vc-make-backup-files t)
  (kept-old-versions 10)
  (kept-new-versions 10)
  
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
       ((if (> (x-display-pixel-width) 2560) 100 120)))
      (set-face-attribute 'default nil :height)))

  (set-height-by-system-name-and-display)

  ;; Supposedly this should call the height setting function when the display monitor changes.
  ;; [TODO] Test this, I don't know if this works.
  (setq display-monitors-changed-functions #'set-height-by-system-name-and-display)

  (global-display-line-numbers-mode 1)

  ;; Softwrap and still kill to end of line instead of killing visual line only
  ;; maybe consider disable the go to beginning or end of visual line too?
  (global-visual-line-mode 1)
  ;; (keymap-unset visual-line-mode-map "C-k")

  ;; Make lambda appear as a symbol
  (global-prettify-symbols-mode 1)

  (unless (and (eq window-system 'mac)
               (bound-and-true-p mac-carbon-version-string))
    ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
    ;; versions, except for emacs-mac.
    ;;
    ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
    ;; this version of Emacs natively supports smooth scrolling.
    ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
    (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
    (pixel-scroll-precision-mode 1)))

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . (lambda ()
                  (let ((args "--group-directories-first -ahlv"))
                    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
                      (if-let* ((gls (executable-find "gls")))
                          (setq insert-directory-program gls)
                        (setq args nil)))
                    (when args
                      (setq dired-listing-switches args)))))
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
;;               ("<backspace>" . #'dired-jump))
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

;; (use-package ido
;;   :ensure nil
;;   :custom
;;   (ido-enable-flex-matching 't)
;;   (ido-everywhere 't)
;;   (ido-use-filename-at-point 'guess)
;;   :config
;;   (ido-mode 1))

;; (use-package icomplete
;;   :ensure nil
;;   :config
;;   ;; (icomplete-mode 1)
;;   (fido-mode 1))

(use-package completion
  :ensure nil
  :demand t
  :bind
  (:map completion-in-region-mode-map
        ("M-n" . minibuffer-next-completion)
        ("M-p" . minibuffer-previous-completion))
  (:map minibuffer-mode-map
        ("M-n" . minibuffer-next-completion)
        ("M-p" . minibuffer-previous-completion))
  :custom
  (completions-max-height 20)
  (completion-auto-select nil)
  :config
  (cl-callf append completion-styles '(substring initials flex)))

(use-package tab-bar
  :ensure nil
  :custom (tab-bar-show 1)
  :config (tab-bar-mode))

(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer
             eglot-format
             eglot-code-actions
             eglot-find-implementation)
  :bind
  (("C-<return> C-k" . eldoc)
   ("C-<return> C-S-r" . xref-find-references)
   ("C-<return> C-<return>" .
    (lambda ()
      (interactive)
      (message "hi this should auto fmt")))
   :map eglot-mode-map
   ("C-<return> C-f" . eglot-format)
   ("C-<return> C-<return>" . eglot-format-buffer)
   ("C-<return> C-a" . eglot-code-actions)
   ("C-<return> C-r" . eglot-rename)
   ("C-<return> C-i" . eglot-find-implementation)))

;; Add to path env, workstation specific
;; TODO write a function to automate this
(let ((exec-path-file "./exec-path.el"))
  (when (file-exists-p exec-path-file)
    (load-file exec-path-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimal emacs recommendations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

(setq auto-save-interval 300)
(setq auto-save-timeout 30)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External packages recommended by minimal emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :config
  (vertico-mode))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;; Embark integrates with Consult and Vertico to provide context-sensitive
;; actions and quick access to commands based on the current selection, further
;; improving user efficiency and workflow within Emacs. Together, they create a
;; cohesive and powerful environment for managing completions and interactions.
(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult offers a suite of commands for efficient searching, previewing, and
;; interacting with buffers, file contents, and more, improving various tasks.
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  ;; (setq consult-async-input-debounce 0.02
  ;;       consult-async-input-throttle 0.05
  ;;       consult-async-refresh-delay 0.02)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;; The built-in outline-minor-mode provides structured code folding in modes
;; such as Emacs Lisp and Python, allowing users to collapse and expand sections
;; based on headings or indentation levels. This feature enhances navigation and
;; improves the management of large files with hierarchical structures.
(use-package outline
  :ensure nil
  :commands outline-minor-mode
  :hook
  ((emacs-lisp-mode . outline-minor-mode)
   ;; Use " ▼" instead of the default ellipsis "..." for folded text to make
   ;; folds more visually distinctive and readable.
   (outline-minor-mode
    .
    (lambda()
      (let* ((display-table (or buffer-display-table (make-display-table)))
             (face-offset (* (face-id 'shadow) (ash 1 22)))
             (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
        (set-display-table-slot display-table 'selective-display value)
        (setq buffer-display-table display-table))))))

;; The easysession Emacs package is a session manager for Emacs that can persist
;; and restore file editing buffers, indirect buffers/clones, Dired buffers,
;; windows/splits, the built-in tab-bar (including tabs, their buffers, and
;; windows), and Emacs frames. It offers a convenient and effortless way to
;; manage Emacs editing sessions and utilizes built-in Emacs functions to
;; persist and restore frames.
(use-package easysession
  :ensure t
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)

  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  ;; Key mappings:
  ;; C-c l for switching sessions
  ;; and C-c s for saving the current session
  (global-set-key (kbd "C-c l") 'easysession-switch-to)
  (global-set-key (kbd "C-c s") 'easysession-save-as)

  ;; The depth 102 and 103 have been added to to `add-hook' to ensure that the
  ;; session is loaded after all other packages. (Using 103/102 is particularly
  ;; useful for those using minimal-emacs.d, where some optimizations restore
  ;; `file-name-handler-alist` at depth 101 during `emacs-startup-hook`.)
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))

;; Org mode is a major mode designed for organizing notes, planning, task
;; management, and authoring documents using plain text with a simple and
;; expressive markup syntax. It supports hierarchical outlines, TODO lists,
;; scheduling, deadlines, time tracking, and exporting to multiple formats
;; including HTML, LaTeX, PDF, and Markdown.
(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  ;; (org-fontify-done-headline t)
  ;; (org-fontify-todo-headline t)
  ;; (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks t)
  (org-startup-truncated t))

;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

;; Automatically generate a table of contents when editing Markdown files
(use-package markdown-toc
  :ensure t
  :commands (markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-delete-toc
             markdown-toc--toc-already-present-p)
  :custom
  (markdown-toc-header-toc-title "**Table of Contents**"))

;; Tree-sitter in Emacs is an incremental parsing system introduced in Emacs 29
;; that provides precise, high-performance syntax highlighting. It supports a
;; broad set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, Markdown, and many others.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; This automates the process of updating installed packages
(use-package auto-package-update
  :ensure t
  :custom
  ;; Set the number of days between automatic updates.
  ;; Here, packages will only be updated if at least 7 days have passed
  ;; since the last successful update.
  (auto-package-update-interval 7)

  ;; Suppress display of the *auto-package-update results* buffer after updates.
  ;; This keeps the user interface clean and avoids unnecessary interruptions.
  (auto-package-update-hide-results t)

  ;; Automatically delete old package versions after updates to reduce disk
  ;; usage and keep the package directory clean. This prevents the accumulation
  ;; of outdated files in Emacs’s package directory, which consume
  ;; unnecessary disk space over time.
  (auto-package-update-delete-old-versions t)

  ;; Uncomment the following line to enable a confirmation prompt
  ;; before applying updates. This can be useful if you want manual control.
  ;; (auto-package-update-prompt-before-update t)

  :config
  ;; Run package updates automatically at startup, but only if the configured
  ;; interval has elapsed.
  (auto-package-update-maybe)

  ;; Schedule a background update attempt daily at 10:00 AM.
  ;; This uses Emacs' internal timer system. If Emacs is running at that time,
  ;; the update will be triggered. Otherwise, the update is skipped for that
  ;; day. Note that this scheduled update is independent of
  ;; `auto-package-update-maybe` and can be used as a complementary or
  ;; alternative mechanism.
  (auto-package-update-at-time "12:30"))

(use-package buffer-terminator
  :ensure t
  :custom
  ;; Enable/Disable verbose mode to log buffer cleanup events
  (buffer-terminator-verbose nil)

  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive (default is 30 minutes):
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes

  ;; Define how frequently the cleanup process should run (default is every 10
  ;; minutes):
  (buffer-terminator-interval (* 10 60)) ; 10 minutes

  :config
  (buffer-terminator-mode 1))

(use-package avy
  :ensure t
  :commands (avy-goto-char
             avy-goto-char-2
             avy-next)
  :init
  (global-set-key (kbd "C-'") 'avy-goto-char-2))

;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :ensure t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

;; The official collection of snippets for yasnippet.
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; YASnippet is a template system designed that enhances text editing by
;; enabling users to define and use snippets. When a user types a short
;; abbreviation, YASnippet automatically expands it into a full template, which
;; can include placeholders, fields, and dynamic content.
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode
             yas-global-mode)

  :hook
  (after-init . yas-global-mode)

  :custom
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
  (setq yas-verbosity 0))

;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

;; Enables automatic indentation of code while typing
(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

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
     "__" "_|_"
     ;; Fira code: 0xFF 0x12
     ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
     ;; The few not covered by the regexps.
     "{|"  "[|"  "]#"  "(*"
     "[TODO]" "[DEBUG]" "[INFO]" "[WARN]" "[ERROR]" "[FIXME]"))
  (global-ligature-mode t))

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

(use-package magit)

(use-package diff-hl
  ;; :custom (diff-hl-disable-on-remote t) ; disable on tramp
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode 1)
  (diff-hl-flydiff-mode 1))

(use-package phi-search
  :bind
  ("C-s" . #'phi-search)
  ("C-r" . #'phi-search-backward))

(use-package region-bindings-mode
  :config (region-bindings-mode-enable))

(use-package multiple-cursors
  :after region-bindings-mode
  :bind (:map region-bindings-mode-map
              ("n" . mc/mark-next-symbol-like-this)
              ("N" . mc/mark-next-like-this)
              ("p" . mc/mark-previous-symbol-like-this)
              ("P" . mc/mark-previous-like-this)
              ("a" . mc/mark-all-symbols-like-this)
              ("A" . mc/mark-all-like-this)
              ("s" . mc/mark-all-in-region-regexp)
              ("S" . mc/mark-all-in-region)
              ("l" . mc/edit-ends-of-lines)
              ("m" . mc/mark-more-like-this-extended)))

;; Use puni-mode globally and disable it for term-mode.
(use-package puni
  :defer t
  :after region-bindings-mode
  :bind (:map region-bindings-mode-map
              ("(" . puni-wrap-round)
              ("[" . puni-wrap-square)
              ("{" . puni-wrap-curly)
              ("<" . puni-wrap-angle))
  :hook  (prog-mode
          sgml-mode
          nxml-mode
          tex-mode
          eval-expression-minibuffer-setup))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package markdown-mode :defer t)
(use-package toml-mode :defer t)
(use-package cmake-mode :defer t)
(use-package kotlin-mode :defer t)
(use-package yaml-mode :defer t)
