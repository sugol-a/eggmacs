(egg:module! glasses
  (defvar egg:glasses-fringe-width 340
    "Window fringe width to add when egg:glasses-mode is enabled.
Can either be a value specified in pixels, or `'adaptive' to size
the fringes adaptively according to
`egg:glasses-adaptive-fringe-metric'")

  (defvar egg:glasses-adaptive-fringe-metric 0.667
    "Proportion of the window to reserve for the buffer content when
egg:glasses-mode is activated")

  (defvar egg:glasses-text-scale 0.8
    "Text scale to apply when egg:glasses-mode is enabled.")

  (defvar egg:glasses-line-spacing 0.1
    "Line spacing to apply when egg:glasses-mode is enabled.")

  (defvar egg:glasses-display-line-numbers nil
    "Set to non-nil to enable line numbers in egg:glasses-mode, nil otherwise")

  (defvar egg:glasses-truncate-lines t
    "Set to non-nil to enable line truncation in egg:glasses-mode, nil otherwise")

  (defvar egg:glasses-keep-point-centered t
    "Set to non-nil to keep point centered in the window when egg:glasses-mode is enabled, nil otherwise")

  (egg:bind-parameters! config
    (:fringe . egg:glasses-fringe-width)
    (:adaptive-metric . egg:glasses-adaptive-fringe-metric)
    (:text-scale . egg:glasses-text-scale)
    (:line-spacing . egg:glasses-line-spacing)
    (:line-numbers . egg:glasses-display-line-numbers)
    (:truncate-lines . egg:glasses-truncate-lines)
    (:center-point . egg:glasses-keep-point-centered))

  (defmacro egg:--glasses-inhibit-adaptive-hook! (&rest body)
    `(progn
       (remove-hook 'window-configuration-change-hook #'egg:--glasses-update-adaptive-fringe t)
       (progn ,@body)
       (add-hook 'window-configuration-change-hook #'egg:--glasses-update-adaptive-fringe nil t)))

  (defun egg:--glasses-update-adaptive-fringe ()
    (let* ((width (window-pixel-width nil))
	   (content-width (floor (* egg:glasses-adaptive-fringe-metric width)))
	   (fringe-size (/ (- width content-width) 2)))
      (egg:--glasses-inhibit-adaptive-hook!
       (set-window-fringes nil fringe-size fringe-size))))

  (defun egg:--glasses-inhibit-split (&rest r)
    (message "Window splitting is disabled while egg:glasses-mode is enabled."))

  (defun egg:--glasses-enable ()
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

    (if (eq egg:glasses-fringe-width 'adaptive)
	(progn 
	  (add-hook 'window-configuration-change-hook
		    #'egg:--glasses-update-adaptive-fringe
		    nil t)
	  (egg:--glasses-update-adaptive-fringe))
      (set-window-fringes nil
			  egg:glasses-fringe-width
			  egg:glasses-fringe-width))

    (text-scale-set egg:glasses-text-scale)
    (toggle-truncate-lines egg:glasses-truncate-lines)
    (setq-local display-line-numbers egg:glasses-display-line-numbers
		line-spacing egg:glasses-line-spacing)

    (when egg:glasses-keep-point-centered t
	  (setq-local scroll-margin most-positive-fixnum
		      maximum-scroll-margin 0.5))

    (delete-other-windows)
    (advice-add #'split-window-below :override #'egg:--glasses-inhibit-split)
    (advice-add #'split-window-right :override #'egg:--glasses-inhibit-split))

  (defun egg:--glasses-disable ()
    (egg:unstash 'egg:glasses)
    (advice-remove #'split-window-below #'egg:--glasses-inhibit-split)
    (advice-remove #'split-window-right #'egg:--glasses-inhibit-split))

  (define-minor-mode egg:glasses-mode
    "Glasses."
    :init-value nil
    :lighter "👓"

    (let ((inhibit-message t))
      (if egg:glasses-mode
	  (egg:--glasses-enable)
	(egg:--glasses-disable)))))
