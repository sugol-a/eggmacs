(defun egg:inside-comment-p ()
  "Return t if cursor is inside a comment, nil otherwise.
   Shamelessly stolen from
   http://xahlee.info/emacs/emacs/elisp_determine_cursor_inside_string_or_comment.html"
  (nth 4 (syntax-ppss)))

(defun egg:extend-comment ()
  (interactive)
  (when (egg:inside-comment-p)
    (let* ((eol (save-excursion
		  (end-of-line)
		  (point)))
	   
	   (bol (save-excursion
		  (beginning-of-line)
		  (point)))
	   
	   (is-end-comment (and (> (length comment-end) 0) (save-excursion
							     (goto-char eol)
							     (backward-char (length comment-end))
							     (looking-at (regexp-quote comment-end)))))
	   
	   (comment-continuation (or comment-continue comment-start))
	   (comment-line-start
	    (save-excursion
	      (beginning-of-line)
	      (or (search-forward comment-start eol t)
		  (search-forward comment-continue eol t)))))
      
      (when comment-line-start
	(let ((comment-column (- comment-line-start bol)))
	  (end-of-line)
	  (newline)
	  (dotimes (_ (- comment-column (length comment-continuation))) (insert " "))
	  (insert comment-continuation)
	  (when is-end-comment
	    (save-excursion
	      (previous-line)
	      (end-of-line)
	      (when (search-backward comment-end bol t)
		(kill-line)))
	    (save-excursion
	      (insert comment-end))))))))

(egg:module! dev
  (egg:feature-gate! +projectile
    (egg:package! projectile
      :init
      (projectile-mode +1))
    
    (dolist (binding (egg:keys! projectile))
      (keymap-global-set (car binding) (cdr binding))))

  (egg:feature-gate! +company
    (egg:package! company
      :config
      (egg:bind-parameters! company
	(:delay . company-idle-delay)
	(:min-prefix . company-minimum-prefix-length))))

  (egg:feature-gate! +flycheck
    (egg:package! flycheck))
  
  (egg:feature-gate! +lsp
    (egg:package! lsp-mode)

    (egg:with-parameter! lsp
      (dolist (hook (plist-get lsp :hooks))
	(add-hook hook #'lsp-mode))))
  
  (egg:feature-gate! '(+lsp +lsp-ui)
    (egg:package! lsp-ui)

    (egg:with-parameter! lsp-ui
      (dolist (feature (plist-get lsp-ui :disable-features))
	(cond ((eq feature 'symbol-highlighting) (setq lsp-enable-symbol-highlighting nil))
	      ((eq feature 'doc-hover-mouse) (setq lsp-ui-doc-show-with-mouse nil))
	      ((eq feature 'doc-hover-cursor (setq lsp-ui-doc-show-with-cursor nil)))
	      ;; TODO: the rest of them lol
	      ))))

  (egg:feature-gate! +treesit
    (egg:package! treesit-auto
      :config (global-treesit-auto-mode)))

  (egg:feature-gate! +rust
    (egg:package! rustic))

  (egg:feature-gate! +typescript
    (push '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
	  treesit-language-source-alist)
    (push '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil)
	  treesit-language-source-alist)

    (add-to-list 'auto-mode-alist '("\\.tsx?" . typescript-ts-mode)))

  (egg:feature-gate! +meson
    (egg:package! meson-mode))

  (egg:feature-gate! '(+python +pipenv)
    (egg:package! pipenv
      :hook (python-mode . pipenv-mode)
      :init (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)))

  (egg:feature-gate! '(+python +pyvenv)
    (egg:package! pyvenv
      :ensure t
      :init (setenv "WORKON_HOME" "~/.local/share/virtualenvs")))

  (egg:feature-gate! '(+python +pyright +lsp)
    (egg:package! lsp-pyright
      :hook (python-mode . (lambda ()
			     (require 'lsp-pyright))))))
