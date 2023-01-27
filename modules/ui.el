(egg:module! ui
  
  (egg:feature-gate! +vertico
    (egg:package! vertico
      :init (vertico-mode))
    (egg:package! savehist
      :init (savehist-mode)))

  (egg:feature-gate! '(+vertico +marginalia)
    (egg:package! marginalia
      :init (marginalia-mode)))

  (egg:feature-gate! +treemacs
    (egg:package! treemacs)

    (when (egg:module-feature 'code '+projectile)
      (egg:package! treemacs-projectile))

    (when (egg:module-feature 'code '+lsp)
      (egg:package! lsp-treemacs))

    (dolist (binding (egg:keys! treemacs))
      (keymap-global-set (car binding) (cdr binding))))

  (egg:feature-gate! +popwin
    (egg:package! popwin
      :init (popwin-mode +1)))

  (egg:feature-gate! +zoom
    (egg:package! zoom
      :init
      (zoom-mode +1)
      :config
      (egg:bind-parameters! zoom
	  (:size . zoom-size))))

  (egg:feature-gate! +which-key
    (egg:package! which-key
      :init
      (which-key-mode +1)
      :config
      (egg:bind-parameters! which-key
	  (:delay . which-key-idle-delay))))

  (egg:with-parameter! theme
    (load-theme theme t))

  (egg:with-parameter! font
    (set-frame-font font t t))

  (egg:with-parameter! variable-pitch-font
    (set-face-attribute 'variable-pitch nil :font variable-pitch-font))

  (egg:with-parameter! smooth-scroll
    (when smooth-scroll
      (setq scroll-conservatively 101)))

  ;; Make yes-or-no prompts use y-or-n
  (fset 'yes-or-no-p #'y-or-n-p))
