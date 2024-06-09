;;; -*- lexical-binding: t; -*-

(egg:defmodule! dev
  (egg:initmodule!
    (egg:feature! +projectile
      (egg:package! projectile
	:init (projectile-mode +1)))

    (egg:feature! +company
      (egg:package! company
	:commands (company-mode)
	:defer t))

    (egg:feature! +corfu
      (egg:package! corfu
        :commands (corfu-mode)
        :defer t))

    (egg:feature! +flycheck
      (egg:package! flycheck
	:commands (flycheck-mode)
	:defer t))

    (egg:feature! +lsp
      (egg:package! lsp-mode)
      (egg:package! lsp-ui)

      (dolist (hook egg:dev/lsp-mode-hooks)
	(add-hook hook #'lsp-mode)))

    (egg:feature! +treesit
      (egg:package! treesit-auto
	:config (global-treesit-auto-mode)))

    (egg:feature! +rainbow-delimiters
      (egg:package! rainbow-delimiters
	:commands (rainbow-delimiters-mode)
	:hook (prog-mode . rainbow-delimiters-mode)
	:defer t))

    (egg:feature! +lang-rust
      (egg:package! rustic
	:defer t))

    (egg:feature! +lang-typescript
      (push '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src" nil nil)
	    treesit-language-source-alist)
      (push '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src" nil nil)
	    treesit-language-source-alist))

    (egg:feature! +lang-meson
      (egg:package! meson-mode
	:defer t))

    (egg:feature! +lang-svelte
      ;; Svelte uses non-treesitter typescript
      (egg:package! typescript-mode
	:defer t)
      (egg:package! svelte-mode
	:defer t))

    (egg:feature! +lang-python
      (egg:feature! +lsp
        (egg:package! lsp-pyright
          :defer t)))

    (egg:feature! +lang-xml
      (egg:feature! +lsp
        (egg:hook! nxml-mode-hook
          (lsp)))))

  (egg:defvar! egg:dev/lsp-mode-hooks '()
    "Major mode hooks to initialize lsp for")

  (egg:defun! egg:dev/inside-comment-p ()
    "Return t if cursor is inside a comment, nil otherwise.
     From http://xahlee.info/emacs/emacs/elisp_determine_cursor_inside_string_or_comment.html"
    (nth 4 (syntax-ppss)))

  (egg:defun! egg:dev/extend-comment ()
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
		(insert comment-end)))))))))
