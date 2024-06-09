;;; -*- lexical-binding: t; -*-

(egg:defmodule! ui
  (setq frame-title-format '(multiple-frames "%b"
					     ("" "%b - Eggmacs @ " system-name)))

  (egg:initmodule!
    (egg:feature! +dashboard
      (setq dashboard-startup-banner egg:ui/dashboard-banner-path)))

  (egg:feature! +dashboard
    (defun egg:--dashboard-mode ()
      (set-face-attribute 'dashboard-text-banner-face nil
                          :weight 'bold)

      (add-to-list 'window-size-change-functions (lambda ()
                                                   (when (string= (buffer-name) dashboard-buffer-name)
                                                     (dashboard-refresh-buffer)))))

    (egg:package! dashboard
      :init (progn
              (setq initial-buffer-choice #'dashboard-open)
              (when (member '+projectile (cdr (assq 'dev egg:modules)))
                (setq dashboard-projects-backend 'projectile))
              (setq dashboard-center-content t
                    dashboard-banner-logo-title "Welcome to Eggmacs"
                    dashboard-startupify-list '(dashboard-insert-banner dashboard-insert-newline dashboard-insert-banner-title dashboard-insert-newline dashboard-insert-init-info dashboard-insert-items))

              (add-hook 'dashboard-mode-hook #'egg:--dashboard-mode))))

  (egg:feature! +completion-vertico
    (egg:package! vertico
      :init (vertico-mode +1)
      :straight (:files (:defaults "extensions/*"))))

  (egg:feature! +completion-vertico-posframe
    (egg:package! vertico-posframe
      :init (vertico-posframe-mode +1)))

  (egg:feature! +completion-marginalia
    (egg:package! marginalia
      :init (marginalia-mode)))

  (egg:feature! +completion-orderless
    (egg:package! orderless
      :init (setq completion-styles '(orderless basic)
		  completion-category-defaults nil
		  completion-category-overrides '((file (styles partial-completion))))))

  (egg:feature! +completion-savehist
    (egg:package! savehist
      :init (savehist-mode)))

  (egg:feature! +nerd-icons
    (egg:package! nerd-icons))

  (egg:feature! +treemacs
    (egg:package! treemacs
      :commands (treemacs)
      :defer t)

    (egg:feature! +nerd-icons
      (egg:package! treemacs-nerd-icons
        :defer t
        :commands (treemacs-load-theme)
        :config (treemacs-load-theme "nerd-icons"))))

  (egg:feature! +treemacs-projectile
    (egg:package! treemacs-projectile
      :defer t))

  (egg:feature! +treemacs-lsp
    (egg:package! lsp-treemacs
      :defer t))

  (egg:feature! +doom-themes
    (egg:package! doom-themes))

  (egg:feature! +base16-themes
    (egg:package! base16-theme))

  (egg:package! popwin
    :init (popwin-mode +1))

  (egg:package! zoom
    :init (zoom-mode +1))

  (egg:package! which-key
    :init (which-key-mode +1))

  (egg:feature! +mood-line
    (egg:package! mood-line
      :config (mood-line-mode +1)))

  (egg:feature! +lambda-line
    (egg:package! lambda-line
      :straight (:type git :host github :repo "lambda-emacs/lambda-line")
      :config
      (lambda-line-mode +1)))

  (egg:package! ace-window
    :defer t)

  (egg:feature! +persp
    (egg:package! persp-mode
      :init (persp-mode +1)
      :defer t))

  (egg:feature! +solaire
    (egg:package! solaire-mode
      :config
      (egg:feature! +treemacs
        (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
        (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
        (solaire-global-mode +1))))

  (egg:feature! +helpful
    (egg:package! helpful
      :config
      (keymap-global-set "C-h f" #'helpful-function)
      (keymap-global-set "C-h v" #'helpful-variable)
      (keymap-global-set "C-h k" #'helpful-key)
      (keymap-global-set "C-h o" #'helpful-symbol)))

  (egg:initmodule!
    (when egg:ui/theme
      (load-theme egg:ui/theme t))

    (when egg:ui/font
      (set-frame-font egg:ui/font t t))

    (setq frame-resize-pixelwise t
	  scroll-conservatively 101
	  truncate-lines t
	  ring-bell-function (lambda () nil))

    (pixel-scroll-precision-mode +1))

  (egg:defvar! egg:ui/theme nil
    "Theme.")

  (egg:defvar! egg:ui/font nil
    "Font.")

  (egg:defvar! egg:ui/mode-line-padding '(2 . 8)
    "Mode line padding")

  (egg:defvar! egg:ui/dashboard-banner-path (concat user-emacs-directory "splash/banner.txt")
    "Dashboard banner path")

  (egg:defvar! egg:ui/variable-pitch-font nil
    "Variable pitch font")

  (egg:defun! egg:--ui/configure-mode-line ()
    (let ((mode-line-bg (face-background 'mode-line))
          (mode-line-inactive-bg (face-background 'mode-line-inactive)))
      (set-face-attribute 'mode-line nil
                          :box `(:line-width
                                 ,egg:ui/mode-line-padding
                                 :color ,mode-line-bg
                                 :style flat-button))
      (set-face-attribute 'mode-line-inactive nil
                          :box `(:line-width
                                 ,egg:ui/mode-line-padding
                                 :color ,mode-line-inactive-bg
                                 :style flat-button)))))
