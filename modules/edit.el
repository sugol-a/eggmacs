(defvar egg:--auto-insert-template-alist ()
  '(("\\.c$" . "This is a test")))

(defun egg:--auto-insert-template ()
  (when-let* ((filename (file-name-nondirectory (buffer-file-name)))
	      (action (cdr (seq-find (lambda (elem)
				       (string-match-p (car elem) filename))
				     egg:--auto-insert-template-alist))))
    (cond ((stringp action) (progn
			      (beginning-of-buffer)
			      (insert action)))
	  ((functionp action) (funcall action)))))

(defun egg:--maybe-auto-insert-template ()
  (when (eq 0 (- (point-max) (point-min)))
    (egg:--auto-insert-template)))

(egg:module! edit
  (defun egg:beginning-of-line-or-text ()
    (interactive)
    (let ((current-position (point)))
      (beginning-of-line-text)
      (when (= current-position (point))
	(beginning-of-line))))

  (defun egg:delete-to-end-of-previous-line ()
    (interactive)
    (let* ((pos (point)))
      (beginning-of-line)
      (delete-char (- pos (point)))
      (backward-delete-char 1)))
  
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
    (egg:package! focus))

  (egg:feature-gate! +autoinsert
    (egg:bind-parameters! autoinsert
      (:templates . egg:--auto-insert-template-alist))
    (add-hook 'find-file-hook #'egg:--maybe-auto-insert-template)))
