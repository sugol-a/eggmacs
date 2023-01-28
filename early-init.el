;; -*- lexical-binding: t; -*- 

(defun egg:--early-straight-setup ()
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

(defun egg:--early-init-cleanup ()
  (setq gc-cons-threshold 8000000))

(defvar egg:modules ()
  "Egg modules to load on init")

(defvar egg:modules-path
  (concat
   (expand-file-name user-emacs-directory)
   "modules")
  "Directory to load Egg modules from")

(defmacro egg:package! (package &rest body)
  (declare (indent 1))
  `(let ((egg:--current-package (quote ,package)))
     (use-package ,package ,@body :straight t)))

(defmacro egg:extend-mode! (extension mode-hook enable &optional disable)
  (declare (indent 1))
  `(progn (define-minor-mode ,extension
	    "Extension mode"
	    :init-value nil
	    (if ,extension
		(,@enable)
	      (,@disable)))
	  (add-hook (quote ,mode-hook) (quote ,extension))))

(defun egg:init ()
  (cl-loop for module in (cons 'egg egg:modules) ; Always load the main module
	   collect (let ((module-symbol (if (consp module)
					  (car module)
					module)))
		     (load (concat egg:modules-path
				   "/"
				   (symbol-name module-symbol))))))

;; Prevent gc cleanup while we initialise
(setq gc-cons-threshold most-positive-fixnum)

;; We don't use package.el
(setq package-enable-at-startup nil)

;; Disable useless UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'before-init-hook #'egg:--early-straight-setup)
(add-hook 'after-init-hook #'egg:--early-init-cleanup)
