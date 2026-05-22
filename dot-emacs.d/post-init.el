(when (native-comp-available-p)
  ;; Native compilation enhances Emacs performance by converting Elisp code into
  ;; native machine code, resulting in faster execution and improved
  ;; responsiveness.
  ;;
  ;; Ensure adding the following compile-angel code at the very beginning
  ;; of your `~/.emacs.d/post-init.el` file, before all other packages.
  (use-package compile-angel
    :diminish compile-angel-on-load-mode
    :demand t
    :ensure t
    :custom
    ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
    ;; Drawback: The minibuffer will not display compile-angel's actions.
    (compile-angel-verbose t)

    :config
    ;; The following directive prevents compile-angel from compiling your init
    ;; files. If you choose to remove this push to `compile-angel-excluded-files'
    ;; and compile your pre/post-init files, ensure you understand the
    ;; implications and thoroughly test your code. For example, if you're using
    ;; the `use-package' macro, you'll need to explicitly add:
    ;; (eval-when-compile (require 'use-package))
    ;; at the top of your init file.
    (push "/init.el" compile-angel-excluded-files)
    (push "/early-init.el" compile-angel-excluded-files)
    (push "/pre-init.el" compile-angel-excluded-files)
    (push "/post-init.el" compile-angel-excluded-files)
    (push "/pre-early-init.el" compile-angel-excluded-files)
    (push "/post-early-init.el" compile-angel-excluded-files)

    ;; A local mode that compiles .el files whenever the user saves them.
    ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

    ;; A global mode that compiles .el files prior to loading them via `load' or
    ;; `require'. Additionally, it compiles all packages that were loaded before
    ;; the mode `compile-angel-on-load-mode' was activated.
    (compile-angel-on-load-mode 1)))

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

;; maybe useless because of `diff`
(defun my/diff-auto-save-file ()
  "Get auto-save #file# difference with current buffer."
  (interactive)
  (diff (make-auto-save-file-name) (current-buffer) nil 'noasync))

;; (use-package completion
;;   :ensure nil
;;   :config
;;   ;; (global-completion-preview-mode 1)
;;   :custom
;;   (completion-styles '(flex basic))
;;   (completions-group t)
;;   (completions-sort 'historical)
;;   (completions-format 'one-column)
;;   (completion-show-help nil)
;;   (completion-ignore-case t))
;;
;; (use-package icomplete
;;   :ensure nil
;;   ;; :bind (:map icomplete-minibuffer-map
;;   ;;             ("M-p" . 'icomplete-backward-completions)
;;   ;;             ("M-n" . 'icomplete-forward-completions)
;;   ;;             ("M-RET" . 'icomplete-force-complete-and-exit))
;;   :custom
;;   (icomplete-in-buffer t)
;;   (icomplete-tidy-shadowed-file-names t)
;;   (icomplete-prospects-height 10)
;;   (icomplete-vertical-mode t)
;;   (icomplete-delay-completions-threshold 0)
;;   (icomplete-compute-delay 0)
;;   (icomplete-max-delay-chars 0)
;;   (icomplete-scroll t)
;;   (icomplete-show-matches-on-no-input t)
;;   :config
;;   (advice-add 'completion-at-point
;;               :after #'minibuffer-hide-completions)
;;   (fido-vertical-mode 1)
;;   ;; (fido-mode 1)
;;   )

;; (use-package ido
;;   :ensure nil
;;   :custom
;;   (ido-use-virtual-buffers t)
;;   (ido-everywhere t)
;;   :config
;;   (ido-mode 1))

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
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
  (corfu-auto t)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-history)
  )

;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :config
  (vertico-mode))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;; ;; Embark integrates with Consult and Vertico to provide context-sensitive
;; ;; actions and quick access to commands based on the current selection, further
;; ;; improving user efficiency and workflow within Emacs. Together, they create a
;; ;; cohesive and powerful environment for managing completions and interactions.
;; (use-package embark
;;   ;; Embark is an Emacs package that acts like a context menu, allowing
;;   ;; users to perform context-sensitive actions on selected items
;;   ;; directly from the completion interface.
;;   :commands (embark-act
;;              embark-dwim
;;              embark-export
;;              embark-collect
;;              embark-bindings
;;              embark-prefix-help-command)
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;
;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))
;;
;; (use-package embark-consult
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

;; Consult offers a suite of commands for efficient searching, previewing, and
;; interacting with buffers, file contents, and more, improving various tasks.
(use-package consult
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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(mapc #'disable-theme custom-enabled-themes)  ; Disable all active themes
(load-theme 'whiteboard t)                    ; Load the built-in theme

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.
(use-package stripspace
  :diminish stripspace-local-mode
  :ensure t
  :commands stripspace-local-mode

  ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean t)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))

;; Set up the Language Server Protocol (LSP) servers using Eglot.
(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer))

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
  :bind
  ("C-c l" . #'org-store-link)
  ("C-c a" . #'org-agenda)
  ("C-c c" . #'org-capture)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  ;; (org-fontify-done-headline t)
  ;; (org-fontify-todo-headline t)
  ;; (org-fontify-whole-heading-line t)
  ;; (org-fontify-quote-and-verse-blocks t)
  (org-startup-truncated t)
  (org-use-speed-commands t))

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

;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :diminish apheleia-mode
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

;; Prevent parenthesis imbalance
(use-package paredit
  :ensure t
  :commands paredit-mode
  :hook
  ((emacs-lisp-mode
    clojuredart-mode
    clojure-mode
    clojurescript-mode
    cider-repl-mode
    fennel-mode)
   . paredit-mode))

;; ;; Displays visible indicators for page breaks
;; (use-package page-break-lines
;;   :ensure t
;;   :commands (page-break-lines-mode
;;              global-page-break-lines-mode)
;;   :hook
;;   (emacs-lisp-mode . page-break-lines-mode))

;; Provides functions to find references to functions, macros, variables,
;; special forms, and symbols in Emacs Lisp
(use-package elisp-refs
  :ensure t
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

;; Configure the `tab-bar-show` variable to 1 to display the tab bar exclusively
;; when multiple tabs are open:
(use-package tab-bar
  :ensure nil
  :custom (tab-bar-show 1)
  :config (tab-bar-mode))

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

(use-package which-key
  :diminish which-key-mode
  :ensure nil ; builtin
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  ;; Allow C-h to trigger which-key before it is done automatically
  (which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; Constrain vertical cursor movement to lines within the buffer
(setq dired-movement-style 'bounded)

(setq wdired-allow-to-change-permissions t)

;; Dired buffers: Automatically hide file details (permissions, size,
;; modification date, etc.) and all the files in the `dired-omit-files' regular
;; expression for a cleaner display.
;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "DEL")
                        (lambda ()
                          (interactive)
                          (find-alternate-file "..")))))

(setq dired-omit-files "")

;; dired: Group directories first
(with-eval-after-load 'dired
  (let ((args "--group-directories-first -ahlv"))
    (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
      (if-let* ((gls (executable-find "gls")))
          (setq insert-directory-program gls)
        (setq args nil)))
    (when args
      (setq dired-listing-switches args))))

;; Enables visual indication of minibuffer recursion depth after initialization.
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Enabled backups save your changes to a file intermittently
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)

;; When tooltip-mode is enabled, certain UI elements (e.g., help text,
;; mouse-hover hints) will appear as native system tooltips (pop-up windows),
;; rather than as echo area messages. This is useful in graphical Emacs sessions
;; where tooltips can appear near the cursor.
(setq tooltip-hide-delay 20)    ; Time in seconds before a tooltip disappears (default: 10)
(setq tooltip-delay 0.4)        ; Delay before showing a tooltip after mouse hover (default: 0.7)
(setq tooltip-short-delay 0.08) ; Delay before showing a short tooltip (Default: 0.1)
(tooltip-mode 1)

;; Configure the built-in Emacs server to start after initialization,
;; allowing the use of the emacsclient command to open files in the
;; current session.
(use-package server
  :ensure nil
  :commands server-start
  :hook
  (after-init . server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff I like
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Absolutely useless key that causes problems in GUI mode
(global-unset-key (kbd "C-z"))

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
  ;; (emacs-startup . display-startup-time)

  ;; automatic tag pair editing in sgml, html, etc
  (sgml-mode . sgml-electric-tag-pair-mode)

  :custom
  (show-trailing-whitespace t)
  (split-width-threshold 200)
  (enable-recursive-minibuffers t)
  (custom-file (make-temp-file "emacs-custom"))
  (display-time-24hr-format t)
  (redisplay-skip-fontification-on-input t)
  (compile-command "")
  (compilation-scroll-output t)
  (display-line-numbers-type 'relative)
  (mode-require-final-newline 'ask)
  (custom-unlispify-tag-names nil)
  (initial-major-mode 'lisp-interaction-mode)

  :config

  ;; Set text height
  (defun set-height-by-system-name-and-display ()
    (thread-last
      (cond
       ;; On macbook air
       ((equal system-type 'darwin) 180)
       ;; Else on work laptop
       ((if (> (x-display-pixel-width) 2560) 160 200)))
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
  (global-prettify-symbols-mode 1))

;; Add to path env, workstation specific
;; TODO write a function to automate this
(let ((exec-path-file "~/.emacs.d/exec-path.el"))
  (when (file-exists-p exec-path-file)
    (load-file exec-path-file)))

(use-package diminish :demand t)

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "r")
  (wgrep-change-readonly-file t))

(use-package zoom
  :diminish zoom-mode
  :custom
  (zoom-mode t)
  (zoom-size '(0.618 . 0.618)))

(use-package magit)

(use-package diff-hl
  :custom
  (diff-hl-show-hunk-mouse-mode t)
  ;; (diff-hl-disable-on-remote t) ; disable on tramp
  :after (magit dired)
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode 1))

(use-package phi-search
  :bind
  ("C-s" . #'phi-search)
  ("C-r" . #'phi-search-backward))

(use-package multiple-cursors
  :custom (mc/always-run-for-all t))

;; ;; Cannot work with boon last i checked
;; (use-package region-bindings-mode
;;   :after multiple-cursors
;;   :bind (:map region-bindings-mode-map
;;               ("a" . 'mc/mark-all-like-this-dwim)
;;               ("p" . 'mc/mark-previous-like-this)
;;               ("n" . 'mc/mark-next-like-this)
;;               ("r" . 'mc/mark-all-in-region-regexp)
;;               ("l" . 'mc/edit-lines))
;;   :custom (mc/edit-lines-empty-lines 'ignore) ;; still don't know what this does
;;   :config (region-bindings-mode-enable))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region)
  ("C-+" . 'er/contract-region))

(use-package eldoc-box)

;; (use-package meow
;;   :after eldoc-box
;;   :bind (:map meow-insert-state-keymap
;;               ("C-g" . 'meow-insert-exit))
;;   :config
;;   (defun meow-ergo-setup ()
;;     ;; -------------------- ;;
;;     ;;      THING TABLE     ;;
;;     ;; -------------------- ;;
;;     (meow-thing-register 'angle
;;                          '(pair ("<") (">"))
;;                          '(pair ("<") (">")))
;;
;;     (setq meow-char-thing-table
;;           '((?f . round)
;;             (?d . square)
;;             (?s . curly)
;;             (?a . angle)
;;             (?r . string)
;;             (?v . paragraph)
;;             (?c . line)
;;             (?x . buffer)
;;
;;             (?V . sentence)
;;             (?C . visual-line)
;;             (?e . defun)))
;;
;;     ;; -------------------- ;;
;;     ;;       MAPPINGS       ;;
;;     ;; -------------------- ;;
;;     (meow-define-keys 'normal
;;                                         ; expansion
;;       '("0" . meow-expand-0)
;;       '("1" . meow-expand-1)
;;       '("2" . meow-expand-2)
;;       '("3" . meow-expand-3)
;;       '("4" . meow-expand-4)
;;       '("5" . meow-expand-5)
;;       '("6" . meow-expand-6)
;;       '("7" . meow-expand-7)
;;       '("8" . meow-expand-8)
;;       '("9" . meow-expand-9)
;;       '("'" . meow-reverse)
;;
;;                                         ; movement
;;       '("i" . meow-prev)
;;       '("k" . meow-next)
;;       '("j" . meow-left)
;;       '("l" . meow-right)
;;
;;       '("y" . meow-search)
;;       '("/" . meow-visit)
;;
;;       '("D" . xref-find-definitions)
;;       '("F" . xref-find-references)
;;
;;       '("Q" . meow-quit)
;;       '("{" . flymake-goto-prev-error)
;;       '("}" . flymake-goto-next-error)
;;                                         ; expansion
;;       '("I" . meow-prev-expand)
;;       '("K" . meow-next-expand)
;;       '("J" . meow-left-expand)
;;       '("L" . meow-right-expand)
;;
;;       '("u" . meow-back-word)
;;       '("U" . meow-back-symbol)
;;       '("o" . meow-next-word)
;;       '("O" . meow-next-symbol)
;;
;;       '("a" . meow-mark-word)
;;       '("A" . meow-mark-symbol)
;;       '("s" . meow-line)
;;       '("S" . meow-goto-line)
;;       '("w" . meow-block)
;;       '("q" . meow-join)
;;       '("g" . meow-grab)
;;       '("G" . meow-pop-grab)
;;       '("m" . meow-swap-grab)
;;       '("M" . meow-sync-grab)
;;       '("p" . meow-cancel-selection)
;;       '("P" . meow-pop-selection)
;;
;;       '("x" . meow-till)
;;       '("z" . meow-find)
;;
;;       '("," . meow-beginning-of-thing)
;;       '("." . meow-end-of-thing)
;;       '("<" . meow-inner-of-thing)
;;       '(">" . meow-bounds-of-thing)
;;
;;                                         ; editing
;;       '("d" . meow-kill)
;;       '("f" . meow-change)
;;       '("t" . meow-delete)
;;       '("c" . meow-save)
;;       '("C" . meow-replace)
;;       '("v" . meow-yank)
;;       '("V" . meow-yank-pop)
;;
;;       '("e" . meow-insert)
;;       '("E" . meow-open-above)
;;       '("r" . meow-append)
;;       '("R" . meow-open-below)
;;
;;       '("h" . meow-undo)
;;       '("H" . meow-undo-in-selection)
;;
;;       ;; '("h" . undo-only)
;;       ;; '("H" . undo-redo)
;;
;;       '("b" . open-line)
;;       '("B" . split-line)
;;       '("[" . indent-rigidly-left-to-tab-stop)
;;       '("]" . indent-rigidly-right-to-tab-stop)
;;
;;                                         ; prefix n
;;       '("nf" . meow-comment)
;;       '("nt" . meow-start-kmacro-or-insert-counter)
;;       '("nr" . meow-start-kmacro)
;;       '("ne" . meow-end-or-call-kmacro)
;;       ;; ...etc
;;
;;                                         ; prefix ;
;;       '(";f" . save-buffer)
;;       '(";F" . save-some-buffers)
;;       '(";d" . meow-query-replace-regexp)
;;       '(";l" . eldoc-box-help-at-point)
;;       '(";L" . eldoc)
;;       '(";a" . eglot-code-actions)
;;       ;; ... etc
;;
;;                                         ; ignore escape
;;       '("<escape>" . ignore)))
;;
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     ;; (meow-motion-overwrite-define-key
;;     ;;  '("j" . meow-next)
;;     ;;  '("k" . meow-prev)
;;     ;;  '("<escape>" . ignore))
;;     (meow-thing-register 'angle
;;                          '(pair ("<") (">"))
;;                          '(pair ("<") (">")))
;;     (setq meow-char-thing-table
;;           '((?\( . round)
;;             (?\[ . square)
;;             (?{ . curly)
;;             (?< . angle)
;;             (?s . string)
;;             (?p . paragraph)
;;             (?l . line)
;;             (?b . buffer)
;;             (?. . sentence)
;;             (?v . visual-line)
;;             (?f . defun)))
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
;;      '("{" . flymake-goto-prev-error)
;;      '("}" . flymake-goto-next-error)
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
;;      '("<escape>" . ignore)
;;      '("/" . xref-find-definitions)
;;      '("?" . xref-find-references)))
;;
;;   (require 'meow)
;;   (meow-ergo-setup)
;;   (meow-global-mode 1))

;; (use-package back-button
;;   :bind (("C-<" . 'back-button-global-backward)
;;          ("C->" . 'back-button-global-forward)
;;          ("C-M-<" . 'back-button-local-backward)
;;          ("C-M->" . 'back-button-local-forward))
;;   :config
;;   (back-button-mode 1))

(use-package avy)

(use-package swiper)

(use-package boon
  :after swiper
  :bind (:map boon-insert-map ("C-g" . 'boon-set-command-state)
              :map boon-command-map ("r" . 'swiper))
  :config
  (require 'boon-qwerty)
  (boon-mode))

(use-package lsp-mode :defer t)

(use-package vc-jj :defer t)

;; For copying org-mode contents into Confluence
(use-package ox-clip :after org :defer t)

(use-package markdown-mode :defer t)
(use-package toml-mode :defer t)
(use-package cmake-mode :defer t)

;; Flutter and dart stuff
(use-package dart-mode :defer t)
(use-package dart-server :defer t)
(use-package flutter :defer t)

;; Clojure
(use-package cider :defer t)
(use-package flycheck-clj-kondo :defer t)

(use-package yaml-mode :defer t)

(use-package dockerfile-mode :defer t)

(use-package protobuf-mode :defer t)

(use-package kotlin-mode :defer t)

(use-package docker :ensure t)

(use-package fennel-mode :defer t)

(use-package typescript-mode :defer t)

(use-package geiser :defer t)
(use-package geiser-mit :defer t)
(use-package geiser-gambit :defer t)

(use-package elpy :defer t)

(use-package wgrep :defer t)

(use-package jtsx :defer t)

(use-package gptel :defer t
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (when (file-exists-p "~/.emacs.d/llm.el")
    (load "~/.emacs.d/llm.el")))

(use-package gptel-magit :defer t)
