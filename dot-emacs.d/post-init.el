;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; Native compilation enhances Emacs performance by converting Elisp code into
;; native machine code, resulting in faster execution and improved
;; responsiveness.
;;
;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(when (native-comp-available-p)
  (use-package compile-angel
    :ensure t
    :demand t
    :custom
    ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
    ;; Drawback: The minibuffer will not display compile-angel's actions.
    (compile-angel-verbose nil)

    :config
    ;; The following directive prevents compile-angel from compiling your init
    ;; files. If you choose to remove this push to `compile-angel-excluded-files'
    ;; and compile your pre/post-init files, ensure you understand the
    ;; implications and thoroughly test your code. For example, if you're using
    ;; `use-package', you'll need to explicitly add `(require 'use-package)` at
    ;; the top of your init file.
    (push "/init.el" compile-angel-excluded-files)
    (push "/early-init.el" compile-angel-excluded-files)
    (push "/pre-init.el" compile-angel-excluded-files)
    (push "/post-init.el" compile-angel-excluded-files)
    (push "/pre-early-init.el" compile-angel-excluded-files)
    (push "/post-early-init.el" compile-angel-excluded-files)

    ;; A local mode that compiles .el files whenever the user saves them.
    (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

    ;; A global mode that compiles .el files before they are loaded.
    (compile-angel-on-load-mode)))

;; Allows hiding or customising minor modes in the modeline
(use-package delight)

(defvar my-space-mode-map (make-sparse-keymap "SPACE"))

(define-minor-mode my-space-mode
  "SPACE"
  :global t)

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil)
  (evil-search-module #'evil-search)
  (evil-magic 'very-magic)
  :bind
  ("C-k" . 'evil-window-up)
  ("C-j" . 'evil-window-down)
  ("C-h" . 'evil-window-left)
  ("C-l" . 'evil-window-right)
  ;; I don't use insert digraph (the default binding)
  ;; Use C-k for moving up in vertico and corfu
  (:map evil-insert-state-map ("C-k" . nil))
  :config
  (evil-mode 1)
  (dolist (state '(normal visual))
    (evil-make-intercept-map
     (evil-get-auxiliary-keymap my-space-mode-map state t t)
     state))
  (evil-define-key '(normal visual) 'global " " my-space-mode-map)
  ;; (evil-define-key '(normal visual) 'global " " 'space-map)
  (evil-define-key '(normal visual) 'global "s" 'avy-goto-char-2))

(use-package evil-visualstar
  :after evil
  :config (global-evil-visualstar-mode))

(use-package evil-collection
  :after evil
  :delight (evil-collection-unimpaired-mode)
  :custom (evil-collection-setup-minibuffer t)
  :hook
  (evil-collection-setup
   .
   (lambda (_mode mode-keymaps &rest _)
     ;; (evil-collection-define-key '(normal visual) 'dired-mode-map " " 'space-map)
     ;; (evil-collection-define-key '(normal visual) 'Man-mode-map " " 'space-map)
     (evil-collection-translate-key 'normal  mode-keymaps
       "SPC" nil)
     (evil-collection-define-key '(normal visual) 'compilation-mode-map
       "J" 'compilation-next-error
       "K" 'compilation-previous-error
       "C-j" nil
       "C-k" nil)))
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :delight
  :config (evil-commentary-mode))

(use-package evil-indent-plus
  :after evil
  :config (evil-indent-plus-default-bindings))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

                                        ;Don't know yet if i want to use this
;; (use-package evil-textobj-tree-sitter
;;   :after evil
;;   :config
;;   ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;; (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;; (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;; ;; You can also bind multiple items and we will match the first one we can find
;; (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

;; (use-package evil-lispy
;;   :after evil
;;   :hook
;;   (lisp-mode
;;    lisp-interaction-mode
;;    emacs-lisp-mode
;;    clojure-mode
;;    clojurescript-mode
;;    clojuredart-mode
;;    clojurec-mode
;;    cider-repl-mode))


;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'after-init-hook #'(lambda()
                               (let ((inhibit-message t))
                                 (recentf-mode 1))))

(with-eval-after-load "recentf"
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'after-init-hook #'save-place-mode)

(use-package magit)

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  ;; (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable Maple Mono ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
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
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Font type
;; [WARN] Won't work if system cannot render this font
(set-face-attribute 'default nil :font "Maple Mono")
;; (set-face-attribute 'default nil :font "Maple Mono")

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

;; (set-face-attribute 'default nil :height (get-height-by-system-name-and-display))

;; Set colour theme
(use-package catppuccin-theme
  :custom (catppuccin-flavor 'latte)
  :config (load-theme 'catppuccin :no-confirm))

;; line numbers
(global-display-line-numbers-mode 1)

;; Display trailing whitespace
(setq-default show-trailing-whitespace t)

;; Set emacs' path variable
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; (use-package lispy
;;                                         ; :vc (:url "https://github.com/enzuru/lispy"
;;                                         ;           :branch "master")
;;   :config
;;   ;; Prevent lispy from overwriting goto function
;;   (define-key lispy-mode-map (kbd "M-.") nil)
;;   (setq lispy-compat '(edebug cider magit-blame-mode))
;;   ;; Enable lispy mode for relevant modes
;;   :hook
;;   (lisp-mode
;;    lisp-interaction-mode
;;    emacs-lisp-mode
;;    clojure-mode
;;    clojurescript-mode
;;    clojuredart-mode
;;    clojurec-mode
;;    cider-repl-mode))

;; Splits horizontally (left-right) when this value is below window width
(setq split-width-threshold 100)

(use-package avy
  :bind ("C-:" . 'avy-goto-char))

(use-package ace-window
  :custom (aw-keys '(?f ?d ?s ?a ?k ?l ?h ?g))
  :bind ("C-x o" . 'ace-window))

;; Bigger active screen
(use-package golden-ratio
  :after ace-window evil
  :delight
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-up)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-down)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-left)
  (add-to-list 'golden-ratio-extra-commands 'evil-window-right)
  (golden-ratio-mode 1))

(use-package corfu
  :commands (corfu-mode global-corfu-mode)
  ;; Optional customizations
  :bind (:map corfu-map
              ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt) ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Taken from minimal emacs
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  :init
  (setq global-corfu-minibuffer
        (lambda ()
          (not (or (bound-and-true-p mct--active)
                   (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map)))))

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;; :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

;; Source: https://systemcrafters.net/live-streams/may-21-2021/
(defun minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a character backward"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

;; Enable Vertico.
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t)  ;; Enable cycling for `vertico-next/previous'
  :bind (:map minibuffer-local-map
              ("<backspace>" . minibuffer-backward-kill)
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :init
  (vertico-mode))

;; A few more useful configurations for corfu
(use-package emacs
  :delight
  (auto-fill-function)
  (visual-line-mode)
  (eldoc-mode)
  :custom

  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(flex orderless basic)) ; flex enables fuzzy finding
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode)
  :config (marginalia-mode))

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
   )
  (:map help-map
        ("B" . embark-bindings)) ;; alternative for `describe-bindings'

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
  (setq consult-async-input-debounce 0.02
        consult-async-input-throttle 0.05
        consult-async-refresh-delay 0.02)

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

;; Set up the Language Server Protocol (LSP) servers using Eglot.
(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :config
  (add-to-list 'eglot-server-programs
               '((dart-mode dart-ts-mode) . ("fvm" "dart" "language-server")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
  ;;                . ("clangd")))
  ;; (cl-callf append
  ;;     eglot-server-programs '((dart-mode . ("fvm" "dart" "language-server"))
  ;;                             ;; ((c++-mode c++-ts-mode c-mode c-ts-mode) . ("clangd-12" "--enable-config"))
  ;;                             ;; ( c++-ts-mode . ("clangd-12" "--enable-config"))
  ;;                             ))
  )

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
  :init
  (setq markdown-command "multimarkdown")

  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

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

;; This package is useful for users who want to disable the mouse to:
;; - Prevent accidental clicks or cursor movements that may unexpectedly change
;;   the cursor position.
;; - Reinforce a keyboard-centric workflow by discouraging reliance on the mouse
;;   for navigation.
(use-package inhibit-mouse
  :delight
  :ensure t
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode)
    (inhibit-mouse-mode 1)))

;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :delight
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

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

;; Prevent Emacs from saving customization information to a custom file
;; [TODO] fucks things up, maybe use make-temp-file instead
;; (setq custom-file null-device)
(setq custom-file (make-temp-file "emacs-custom"))

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;; Display of line numbers in the buffer:
(setq-default display-line-numbers-type 'relative)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(use-package which-key
  :ensure nil                           ; builtin
  :delight
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
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

;; Display the time in the modeline
(add-hook 'after-init-hook #'display-time-mode)

;; Paren match highlighting
(add-hook 'after-init-hook #'show-paren-mode)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(add-hook 'after-init-hook #'winner-mode)

;; Replace selected text with typed text
(delete-selection-mode 1)

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

;; Dired buffers: Automatically hide file details (permissions, size,
;; modification date, etc.) and all the files in the `dired-omit-files' regular
;; expression for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Enables visual indication of minibuffer recursion depth after initialization.
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Enabled backups save your changes to a file intermittently
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)

;; Improve Emacs responsiveness by deferring fontification during input
;;
;; NOTE: This may cause delayed syntax highlighting in certain cases
(setq redisplay-skip-fontification-on-input t)

(setq compile-command "")

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Do I need this?
(setenv "ANDROID_HOME" (concat (getenv "HOME") "/Android/Sdk"))

;; Autopairs
(electric-pair-mode 1)

;; Softwrap and still kill to end of line instead of killing visual line only
;; maybe consider disable the go to beginning or end of visual line too?
(global-visual-line-mode 1)
(keymap-unset visual-line-mode-map "C-k")

(setq dired-kill-when-opening-new-dired-buffer t)

;; (use-package format-all)

(use-package diff-hl
  :after magit
  :config
  (global-diff-hl-mode)
  :hook
  (dired-mode . diff-hl-dired-mode)
  ((text-mode prog-mode) . diff-hl-flydiff-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package cider)

(use-package dart-mode)

(use-package cmake-mode)

(use-package dockerfile-mode)

(use-package rainbow-delimiters
  :hook prog-mode)

(define-prefix-command 'file-map)
(keymap-set file-map "c" 'compile)
(keymap-set file-map "C" 'recompile)
(keymap-set file-map "r" 'recentf)
(keymap-set file-map "f" 'find-file)

(define-prefix-command 'kill-map)
(keymap-set kill-map "w" 'delete-window)
(keymap-set kill-map "k" 'kill-buffer-and-window)
(keymap-set kill-map "b" 'kill-current-buffer)

;; (define-prefix-command 'my-space-mode-map)
(keymap-set my-space-mode-map "p" project-prefix-map)
(keymap-set my-space-mode-map "v" vc-prefix-map)
(keymap-set my-space-mode-map "f" 'file-map)
(keymap-set my-space-mode-map "k" 'kill-map)
(keymap-set my-space-mode-map "c" mode-specific-map)
(keymap-set my-space-mode-map "s" search-map)
(keymap-set my-space-mode-map "h" help-map)
(keymap-set my-space-mode-map "b" 'consult-buffer)
(keymap-set my-space-mode-map "B" 'list-buffers)
(keymap-set my-space-mode-map "SPC" 'eglot-format-buffer)
(keymap-set my-space-mode-map "r" 'eglot-rename)
;; (keymap-set my-space-mode-map "e" 'consult-flymake)
;; (keymap-set my-space-mode-map "E" 'consult-compile-error)

;; (define-prefix-command 'space-map)
;; (keymap-set space-map "p" project-prefix-map)
;; (keymap-set space-map "v" vc-prefix-map)
;; (keymap-set space-map "f" 'file-map)
;; (keymap-set space-map "k" 'kill-map)
;; (keymap-set space-map "c" mode-specific-map)
;; (keymap-set space-map "s" search-map)
;; (keymap-set space-map "h" help-map)
;; (keymap-set space-map "b" 'consult-buffer)
;; (keymap-set space-map "B" 'list-buffers)
;; (keymap-set space-map "SPC" 'eglot-format-buffer)
;; (keymap-set space-map "r" 'eglot-rename)
;; ;; (keymap-set space-map "e" 'consult-flymake)
;; ;; (keymap-set space-map "E" 'consult-compile-error)

;; [TODO] add hooks instead?
(require 'backtrace)
(dolist (mode-map (list help-mode-map backtrace-mode-map))
  (keymap-set mode-map "SPC" 'space-map))

;; (use-package comint
;;   :custom (comint-buffer-maximum-size (* 1024 1024)))

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 100)
