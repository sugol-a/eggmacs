(egg:module! edit

  (defun egg:beginning-of-line-or-text ()
    (interactive)
    (let ((current-position (point)))
      (beginning-of-line-text)
      (when (= current-position (point))
	(beginning-of-line))))
  
  (egg:feature-gate! +bol-or-text
    (keymap-global-set "C-a" #'egg:beginning-of-line-or-text))
  
  (egg:feature-gate! +expand
    (egg:package! expand-region)
    
    (dolist (binding (egg:keys! expand))
      (keymap-global-set (car binding) (cdr binding))))

  (egg:feature-gate! +smartparens
    (egg:package! smartparens
      :init (smartparens-global-mode))

    (dolist (binding (egg:keys! smartparens))
      (keymap-global-set (car binding) (cdr binding))))

  (egg:feature-gate! +mc
    (egg:package! multiple-cursors)

    (dolist (binding (egg:keys! mc))
      (keymap-global-set (car binding) (cdr binding))))

  (egg:feature-gate! +snippet
    (egg:package! yasnippet
      :init (yas-global-mode +1))
    (egg:package! yasnippet-snippets))

  (egg:feature-gate! +focus
    (egg:package! focus)))
