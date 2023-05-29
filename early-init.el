;; -*- lexical-binding: t; -*- 
(require 'comp)

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

(defmacro egg:extend-mode! (mode enable &rest flags)
  (declare (indent 1))
  (let ((minor-mode-symbol (gensym (concat (symbol-name mode) "-extension-mode-"))))
    `(progn (define-minor-mode ,minor-mode-symbol
	      ,(concat "Extension mode for " (symbol-name mode))
	      :init-value nil
	      (if ,minor-mode-symbol
		  (,@enable)
		(,@(plist-get flags :disable))))
	    ,(if (plist-get flags :hook)
		 `(add-hook (quote ,mode) (lambda () (,minor-mode-symbol 1)))
	       `(add-hook 'after-change-major-mode-hook
			  (lambda ()
			    (when (eq major-mode (quote ,mode))
			      (,minor-mode-symbol 1))))))))

(defvar egg:--machine-local-vars nil
  "Configuration for this specific machine")

(defmacro egg:define-machines! (&rest machines)
  (cons 'progn
	(when-let ((machine-config (alist-get (system-name) machines nil nil #'string=)))
	  (cons
	   (let ((machine-vars (plist-get machine-config :vars)))
	     `(setq egg:--machine-local-vars ,machine-vars))
	   (when-let ((init (plist-get machine-config :init)))
	     (list init))))))

(defmacro egg:machine-var! (property)
  (declare (indent 1))
  `(plist-get egg:--machine-local-vars ,property))

(defmacro egg:define-keys! (keymaps &rest key-groups)
  "I think I had a stroke."
  (declare (indent 1))
  (cons 'progn
	(cons
	 (cons 'progn
	       (mapcar (lambda (keymap)
			 (let ((keymap-symbol (car keymap))
			       (keymap-description (cdr keymap)))
			   `(defvar ,keymap-symbol (make-sparse-keymap) ,keymap-description)))
		       keymaps))
	 (mapcar
	  (lambda (group)
	    (let ((group-head (car group))
		  (group-bindings (cdr group)))
	      (cons 'progn
		    (cond ((eq :global group-head)
			   (mapcar (lambda (binding)
				     (let ((key (car binding))
					   (definition (cdr binding)))
				       `(keymap-global-set ,key ,definition)))
				   group-bindings))
			  ((eq :hook group-head)
			   (let* ((hook (car group-bindings))
				  (group-bindings (cdr group-bindings)))
			     `((add-hook ',hook (lambda ()
						  ,@(mapcar (lambda (binding)
							      (let ((key (car binding))
								    (definition (cdr binding)))
								`(keymap-local-set ,key ,definition)))
							    group-bindings))))))
			  ((eq :keymap group-head)
			   (let ((keymap (car group-bindings))
				 (group-bindings (cdr group-bindings)))
			     (mapcar (lambda (binding)
				       (let ((key (car binding))
					     (definition (cdr binding)))
					 `(keymap-set ,keymap ,key ,definition)))
				     group-bindings)))
			  (t nil)))))
	  key-groups))))

(defmacro egg:interactive! (args &rest body)
  (declare (indent 1))
  `(lambda ,args (interactive) ,@body))

(defun egg:init ()
  (cl-loop for module in (cons 'egg egg:modules) ; Always load the main module
	   collect (let ((module-symbol (if (consp module)
					  (car module)
					module)))
		     (load (concat egg:modules-path
				   "/"
				   (symbol-name module-symbol))))))

(defun egg:show-startup-time ()
  (message "eggmacs started in %s" (emacs-init-time)))

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

(advice-add 'display-startup-echo-area-message :override #'egg:show-startup-time)
