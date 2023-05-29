(egg:module! glasses
  (defvar egg:glasses-fringe-width 340
    "Fringe width (in pixels) to add when egg:glasses-mode is enabled.")

  (defvar egg:glasses-text-scale 0.8
    "Text scale to apply when egg:glasses-mode is enabled.")

  (defvar egg:glasses-lines-spacing 0.1
    "Line spacing to apply when egg:glasses-mode is enabled.")

  (defvar egg:glasses-display-line-numbers nil
    "Set to non-nil to enable line numbers in egg:glasses-mode, nil otherwise")

  (defvar egg:glasses-truncate-lines t
    "Set to non-nil to enable line truncation in egg:glasses-mode, nil otherwise")

  (defvar egg:glasses-keep-point-centered t
    "Set to non-nil to keep point centered in the window when egg:glasses-mode is enabled, nil otherwise")

  (define-minor-mode egg:glasses-mode
    "Glasses."
    :init-value nil
    :lighter "👓"

    (let ((inhibit-message t))
      (if egg:glasses-mode
	  (progn
	    (egg:stash 'egg:glasses
		       `(set-window-fringes . ,(cons nil (window-fringes)))
		       `(text-scale-set . ,(list text-scale-mode-amount))
		       `(set-window-configuration . ,(list (current-window-configuration)))
		       `(toggle-truncate-lines . ,(list truncate-lines))
		       'line-spacing
		       'display-line-numbers
		       'scroll-margin
		       'maximum-scroll-margin
		       'mode-line-format)
	    (set-window-fringes nil
				egg:glasses-fringe-width
				egg:glasses-fringe-width)
	    (text-scale-set egg:glasses-text-scale)
	    (delete-other-windows)
	    (toggle-truncate-lines egg:glasses-truncate-lines)
	    (setq-local display-line-numbers egg:glasses-display-line-numbers
			line-spacing egg:glasses-line-spacing)

	    (when egg:glasses-center-point
	      (setq-local scroll-margin most-positive-fixnum
			  maximum-scroll-margin 0.5)))

	(egg:unstash 'al:glasses))))
  
