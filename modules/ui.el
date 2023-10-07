;;; -*- lexical-binding: t; -*-

(egg:defmodule! ui
  (setq frame-title-format '(multiple-frames "%b"
					     ("" "%b - Eggmacs @ " system-name)))

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

  (egg:feature! +treemacs
    (egg:package! treemacs
      :commands (treemacs)
      :defer t))

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

  (egg:package! mood-line
    :config (mood-line-mode +1))

  (egg:package! ace-window)

  (egg:package! persp-mode
    :init (persp-mode +1))

  (egg:initmodule!
    (when egg:ui/theme
      (load-theme egg:ui/theme t))

    (when egg:ui/font
      (set-frame-font egg:ui/font t t))

    (setq frame-resize-pixelwise t
	  scroll-conservatively 101
	  truncate-lines t
	  ring-bell-function (lambda () nil))

    (pixel-scroll-precision-mode +1)

    (egg:--ui/configure-mode-line))

  (egg:defvar! egg:ui/theme nil
    "Theme.")

  (egg:defvar! egg:ui/font nil
    "Font.")

  (egg:defvar! egg:ui/mode-line-padding '(2 . 8)
    "Mode line padding")

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

;; (egg:module! ui
;;   (setq frame-title-format '(multiple-frames "%b"
;; 					     ("" "%b - Eggmacs @ " system-name)))

;;   (egg:feature-gate! +vertico
;;     (egg:package! vertico
;;       :init (vertico-mode))
;;     (egg:package! savehist
;;       :init (savehist-mode)))

;;   (egg:feature-gate! '(+vertico +marginalia)
;;     (egg:package! marginalia
;;       :init (marginalia-mode)))

;;   (egg:feature-gate! +treemacs
;;     (egg:package! treemacs)

;;     (when (egg:module-feature 'code '+projectile)
;;       (egg:package! treemacs-projectile))

;;     (when (egg:module-feature 'code '+lsp)
;;       (egg:package! lsp-treemacs))

;;     (dolist (binding (egg:keys! treemacs))
;;       (keymap-global-set (car binding) (cdr binding))))

;;   (egg:feature-gate! +popwin
;;     (egg:package! popwin
;;       :init (popwin-mode +1)))

;;   (egg:feature-gate! +zoom
;;     (egg:package! zoom
;;       :init
;;       (zoom-mode +1)
;;       :config
;;       (egg:bind-parameters! zoom
;; 	(:size . zoom-size))))

;;   (egg:feature-gate! +which-key
;;     (egg:package! which-key
;;       :init
;;       (which-key-mode +1)
;;       :config
;;       (egg:bind-parameters! which-key
;; 	(:delay . which-key-idle-delay))))

;;   (egg:feature-gate! +mood-line
;;     ;; (defun egg:--mood-line-better-format (left right)
;;     ;;   (let ((reserve-right (length right))
;;     ;; 	    (reserve-left (length left)))
;;     ;; 	(concat (propertize " "
;;     ;; 			    'display
;;     ;; 			    `((space :align-to (- left left-margin))))
;;     ;; 		left
;;     ;; 		" "
;;     ;; 		(propertize " "
;;     ;; 			    'display
;;     ;; 			    `((space :align-to (- right
;;     ;; 						  (- 0 right-margin)
;;     ;; 						  ,reserve))))
;;     ;; 		right)))

;;     (egg:package! mood-line
;;       :config
;;       (mood-line-mode)
;;       ;; (advice-add #'mood-line--format :override #'egg:--mood-line-better-format)
;;       ))

;;   (egg:with-parameter! theme
;;     (load-theme theme t))

;;   (egg:with-parameter! font
;;     (set-frame-font font t t))

;;   (egg:with-parameter! variable-pitch-font
;;     (set-face-attribute 'variable-pitch nil :font variable-pitch-font))

;;   (egg:with-parameter! smooth-scroll
;;     (when smooth-scroll
;;       (setq scroll-conservatively 101)))

;;   ;; Make yes-or-no prompts use y-or-n
;;   (fset 'yes-or-no-p #'y-or-n-p))
