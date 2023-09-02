;;; -*- lexical-binding: t; -*-

(egg:defmodule! edit
  (egg:initmodule!
    (egg:feature! +expand-region
      (egg:package! expand-region))

    (egg:feature! +multiple-cursors
      (egg:package! multiple-cursors))

    (egg:feature! +smartparens
      (egg:package! smartparens
        :hook (prog-mode . smartparens-mode)))

    (egg:feature! +snippets
      (egg:package! yasnippet
	:defer t)
      (egg:package! yasnippet-snippets
	:defer t)))
  
  (egg:defun! egg:edit/beginning-of-line-or-text ()
    (interactive)
    (let ((position (point)))
      (beginning-of-line-text)
      (when (= position (point))
	(beginning-of-line))))

  (egg:defun! egg:edit/delete-to-end-of-previous-line ()
    (interactive)
    (let ((position (point)))
      (beginning-of-line)
      (delete-char (- pos (point)))
      (backward-delete-char 1))))
