;;; -*- lexical-binding: t; -*-

(setq user-full-name "Alister Sanders"
      user-mail-address "alister@sugol.org"
      custom-file "/dev/null"
      backup-inhibited t
      read-process-output-max (* 128 1024))

(egg:package! benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

(egg:package! lice
  :commands lice
  :defer t
  :config (setq lice:default-license "mit"))

(egg:use-modules!
 (ui
  +completion-vertico
  +completion-vertico-posframe
  +completion-marginalia
  +completion-savehist
  +completion-orderless

  +treemacs
  +treemacs-projectile
  +treemacs-lsp)

 (edit
  +expand-region
  +multiple-cursors
  +smartparens
  +snippets)

 (dev
  +projectile
  +company
  +flycheck
  +lsp
  +treesit

  +lang-rust
  +lang-python
  +lang-typescript
  +lang-meson
  +lang-svelte))

(egg:hook! prog-mode-hook
  (setq display-line-numbers t)
  (indent-tabs-mode -1)
  (yas-minor-mode +1))

(egg:hook! emacs-lisp-mode-hook
  (company-mode +1))

(setq js-jsx-syntax t)
(setq-default display-line-numbers-width 4)

(setq vertico-posframe-width 120)

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

(egg:hook! c-mode-common-hook
  (c-set-style "bsd")
  (c-set-offset 'case-label '+)
  (c-set-offset 'access-label '-)
  (c-set-offset 'inclass #'al/c-lineup)
  (setq c-basic-offset 4))

(defvar al/frame-keymap
  (define-keymap
    "f" #'toggle-frame-fullscreen
    "z" #'zoom-mode))

(defvar al/multiple-cursors-keymap
  (define-keymap
    "l" #'mc/edit-lines
    "i" #'mc/insert-numbers
    "c" #'mc/insert-letters))

(defvar al/persp-mode-keymap
  (define-keymap
    "p" #'persp-switch
    "c" #'persp-add-new))

(setq persp-keymap-prefix (kbd "C-c M-p"))
(setq lsp-keymap-prefix "C-c l")

(egg:global-keys!
 ("C-c h" . al/frame-keymap)
 ("M-2" . #'split-window-below)
 ("M-3" . #'split-window-right)
 ("M-0" . #'delete-window)
 ("M-o" . #'ace-window)
 ("C-c p" . #'projectile-command-map)
 ("C-c o p" . #'treemacs)
 ("C-=" . #'er/expand-region)
 ("C-c m" . al/multiple-cursors-keymap)
 ("C-x b" . #'persp-switch-to-buffer)
 ("C-c d" . #'duplicate-dwim))

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
        svelte-mode-hook))

(egg:hook! after-init-hook
  (treemacs-project-follow-mode +1))

(egg:hook! projectile-find-file-hook (persp-add-or-not-on-find-file))

(egg:defmachine! macbook
  "Alisters-MacBook-Pro.local"
  :init
  (progn
    (egg:use-feature! ui +base16-themes)

    ;; Swap ⌘ and ⌥
    (setq ns-command-modifier 'meta
          ns-alternate-modifier 'super)

    (setq egg:ui/theme 'base16-black-metal-bathory
          egg:ui/font "Space Mono 18"
          base16-theme-distinct-fringe-background nil)))

(egg:init)
