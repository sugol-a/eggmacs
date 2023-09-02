;; -*- lexical-binding: t; -*- 
(require 'comp)

(defun egg:--core/early-straight-setup ()
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package))

(defun egg:--core/early-init-cleanup ()
  (setq gc-cons-threshold 8000000))

(defmacro egg:package! (package &rest body)
  (declare (indent 1))
  `(let ((egg:--current-package (quote ,package)))
     (use-package ,package ,@body :straight t)))

(defun egg:show-startup-time ()
  (interactive)
  (message "eggmacs started in %s" (emacs-init-time)))

;; Prevent gc cleanup while we initialise
(setq gc-cons-threshold most-positive-fixnum)

;; We don't use package.el
(setq package-enable-at-startup nil)

;; Disable useless UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(load (file-name-concat user-emacs-directory "modules" "egg") t)

(add-hook 'before-init-hook #'egg:--core/early-straight-setup)
(add-hook 'after-init-hook #'egg:--core/early-init-cleanup)

(advice-add 'display-startup-echo-area-message :override #'egg:show-startup-time)
