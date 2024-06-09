;;; -*- lexical-binding: t; -*-

(setq user-full-name "Alister Sanders"
      user-mail-address "alister@sugol.org"
      custom-file "/dev/null"
      backup-inhibited t
      read-process-output-max (* 128 1024))

(egg:use-modules!
 (ui
  +dashboard
  +completion-vertico
  ;; +completion-vertico-posframe
  +completion-marginalia
  +completion-savehist
  +completion-orderless
  +consult

  +treemacs
  ;; +treemacs-projectile
  +treemacs-lsp
  ;; +persp
  )

 (edit
  ;; +expand-region
  +expreg
  +multiple-cursors
  +smartparens
  +snippets
  +whitespace)

 (dev
  +projectile
  +company
  +flycheck
  +lsp
  +treesit
  +rainbow-delimiters

  +lang-rust
  +lang-python
  +lang-typescript
  +lang-meson
  +lang-svelte
  +lang-php
  +lang-web))

(defmacro al:unwave! (&rest faces)
  (declare (indent 0))
  (cons 'progn
        (mapcar (lambda (face)
                  `(let ((underline (face-attribute (quote ,face) :underline)))
                     (set-face-attribute (quote ,face) nil :underline (plist-put underline :style 'line))))
                faces)))

(egg:hook! after-init-hook
  (al:unwave! flycheck-info
              flycheck-error
              flycheck-warning))

(egg:defmachine! macbook
  "Alisters-MacBook-Pro.local"
  :init
  (progn
    (egg:use-feature! ui +base16-themes)

    ;; Swap ⌘ and ⌥
    (setq ns-command-modifier 'meta
          ns-alternate-modifier 'super)
 
    ;; (setq egg:ui/theme 'base16-black-metal-mayhem
    ;;       egg:ui/font "Space Mono 18"
    ;;       base16-theme-distinct-fringe-background nil)

    (egg:package! catppuccin-theme)
    (setq egg:ui/theme 'catppuccin
          egg:ui/font "Space Mono 16")

    (setq lsp-clangd-binary-path "/opt/homebrew/opt/llvm/bin/clangd"
          sql-postgres-program "/opt/homebrew/opt/postgresql@15/bin/psql")

    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
    (add-to-list 'exec-path "/opt/homebrew/bin")))

(egg:defmachine! kronos
  "kronos"
  :init
  (progn
    (egg:use-feature! ui +base16-themes)
    (setq egg:ui/theme 'base16-black-metal-bathory
          egg:ui/font "Space Mono 12"
          base16-theme-distinct-fringe-background nil)

    (setq lsp-disabled-clients '(ccls))
    (setq lsp-clangd-binary-path "/usr/bin/clangd")
    ;; (egg:package! ccls)
    ))

(egg:defmachine! quark
  "quark"
  :init
  (progn
    (egg:use-feature! ui +base16-themes)
    (setq egg:ui/theme 'base16-black-metal-burzum
          egg:ui/font "IBM Plex Mono 11"
          base16-theme-distinct-fringe-background nil)

    (setq lsp-clangd-binary-path "/usr/bin/clangd")))

(egg:defmachine! thinkbook
  "thinkbookpro"
  :init
  (progn
    (egg:use-feature! ui +base16-themes)
    (setq egg:ui/theme 'base16-catppuccin-mocha
          egg:ui/font "IBM Plex Mono 12"
          base16-theme-distinct-fringe-background nil)

    (setq lsp-clangd-binary-path "/usr/bin/clangd")))
(egg:init)

(egg:package! benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

(egg:package! lice
  :commands lice
  :defer t
  :config (setq lice:default-license "mit"))

(egg:hook! prog-mode-hook
  (setq display-line-numbers t)
  (indent-tabs-mode -1)
  (yas-minor-mode +1))

;; (egg:hook! emacs-lisp-mode-hook
;;   (company-mode +1))
;; (push ("\\[jt]sx?" . #'tsx-ts-mode) 'auto-mode-alist)

(setq-default display-line-numbers-width 4)

;; (setq vertico-posframe-width 120)

(defun al/setup-js-indent ()
  (setq-local tab-width 2
              js-indent-level 2
              js-switch-indent-offset 2
              typescript-ts-mode-indent-offset 2))

(egg:hook! js-mode-hook (al/setup-js-indent))
(egg:hook! js-ts-mode-hook (al/setup-js-indent))
(egg:hook! typescript-ts-mode-hook (al/setup-js-indent))
(egg:hook! tsx-ts-mode-hook (al/setup-js-indent))

(defun al/c-lineup (langelem)
  (let ((inclass (assoc 'inclass c-syntactic-context)))
    (save-excursion
      (c-beginning-of-defun)
      (if (or (looking-at "struct")
              (looking-at "typedef struct")
              (looking-at "union")
              (looking-at "typedef union"))
          '+
        '++))))

(defun al/c-setup ()
  (c-set-style "bsd")
  (c-set-offset 'case-label '+)
  (c-set-offset 'access-label '-)
  (c-set-offset 'inclass #'al/c-lineup)
  (setq c-basic-offset 4))

(egg:hook! c-mode-common-hook
  (al/c-setup))

(egg:hook! c++-mode-hook
  (al/c-setup))

(defun al:symbol-extents ()
  (save-excursion
    (thing-at-point--beginning-of-symbol)
    (let ((begin (point)))
      (forward-symbol 1)
      (cons begin (point)))))

(defun al:xml/make-tag-at-symbol ()
  (interactive)
  (save-excursion
    (let* ((extents (al:symbol-extents))
           (begin (car extents))
           (end (cdr extents)))
      (save-restriction
        (narrow-to-region begin end)
        (beginning-of-buffer)
        (insert "<")
        (end-of-buffer)
        (insert ">")))))

(egg:hook! nxml-mode-hook
  (keymap-local-set "C-c C-c" #'al:xml/make-tag-at-symbol))

(defvar al/frame-keymap
  (define-keymap
    "f" #'toggle-frame-fullscreen
    "z" #'zoom-mode))

(defvar al/multiple-cursors-keymap
  (define-keymap
    "l" #'mc/edit-lines
    "i" #'mc/insert-numbers
    "c" #'mc/insert-letters
    ";" #'mc/mark-next-like-this))

(defvar al/persp-mode-keymap
  (define-keymap
    "p" #'persp-switch
    "c" #'persp-add-new))

(setq lsp-keymap-prefix "C-c l")

(egg:global-keys!
 ("C-c h" . al/frame-keymap)
 ("M-2" . #'split-window-below)
 ("M-3" . #'split-window-right)
 ("M-0" . #'delete-window)
 ("M-o" . #'ace-window)
 ("C-c p" . #'projectile-command-map)
 ("C-c o p" . #'treemacs)
 ("C-=" . #'expreg-expand)
 ("C-c m" . al/multiple-cursors-keymap)
 ;; ("C-x b" . #'persp-switch-to-buffer)
 ("C-c d" . #'duplicate-dwim)
 ;; ("C-c M-p" . #'persp-key-map)
 ("C-a" . #'egg:edit/beginning-of-line-or-text)

 ("C-x b" . #'consult-buffer)
 ("M-#" . #'consult-register-load)
 ("M-'" . #'consult-register-store)
 ("C-M-'" . #'consult-register)
 ("M-y" . #'consult-yank-pop)
 ("M-g g" . #'consult-goto-line)
 ("M-g M-g" . #'consult-goto-line)
 ("M-g e" . #'consult-compile-error)
 ("M-s M-s" . #'consult-ripgrep)
 ("C-s" . #'consult-line))

(fset #'yes-or-no-p #'y-or-n-p)

(setq egg:dev/lsp-mode-hooks
      '(c-mode-hook
        c++-mode-hook
        python-mode-hook
        js-mode-hook
        js-ts-mode-hook
        js-jsx-mode-hook
        typescript-ts-mode-hook
        tsx-ts-mode-hook
        svelte-mode-hook
        php-mode))

(setq-default lsp-ui-doc-show-with-mouse nil
              lsp-ui-doc-show-with-cursor t)

(setq dashboard-items '((projects . 5) (recents . 5) (bookmarks . 5) (agenda . 5)))

(with-eval-after-load 'lsp-headerline
  (al:unwave! lsp-headerline-breadcrumb-symbols-hint-face
              lsp-headerline-breadcrumb-symbols-info-face
              lsp-headerline-breadcrumb-symbols-warning-face
              lsp-headerline-breadcrumb-symbols-error-face
              lsp-headerline-breadcrumb-path-hint-face
              lsp-headerline-breadcrumb-path-info-face
              lsp-headerline-breadcrumb-path-warning-face
              lsp-headerline-breadcrumb-path-error-face))

;; (treemacs-project-follow-mode +1)
(setq treemacs-file-event-delay 100)

(egg:hook! projectile-find-file-hook (persp-add-or-not-on-find-file))
(setq persp-auto-resume-time -1         ;Don't autoload buffers
      persp-set-last-persp-for-new-frames nil)

(egg:package! origami)
(add-hook 'prog-mode-hook #'origami-mode)
(keymap-set prog-mode-map "C-c C-SPC" #'origami-toggle-node)

(egg:hook! lsp-mode-hook
  (keymap-local-set "C-c M-." #'lsp-ui-peek-find-references)
  (keymap-local-set "M-." #'lsp-ui-peek-find-definitions))

(egg:hook! prog-mode-hook
  (subword-mode +1)
  (hl-line-mode))

(egg:hook! php-mode-hook
  (lsp))
