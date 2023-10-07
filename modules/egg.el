;;; -*- lexical-binding: t; -*-

(defvar egg:alert-buffer-name "🥚")

(defvar egg:--machines
  '((default .
	     (:init nil :vars nil :id nil :id-function (lambda (&REST _) t)))))

(defvar egg:this-machine nil
  "Identifying symbol for the current machine")

(defvar egg:this-config nil
  "Configuration for the current machine")

(defun egg:identify-by-hostname (identifier)
  (string-equal (system-name) identifier))

(defun egg:--core/identify-this-machine ()
  (if-let ((machine-config
	 (seq-find
	  (lambda (machine)
	    (when-let
		((machine-id (plist-get (cdr machine) :id))
		 (machine-id-function (plist-get (cdr machine) :id-function)))
	      (funcall machine-id-function machine-id)))
	  egg:--machines
	  nil)))
      machine-config
    (alist-get 'default egg:--machines)))

(defun egg:--core/configure-machine (machine-config)
  (let ((init-function (plist-get machine-config :init)))
    (when (functionp init-function)
      (funcall init-function))))

(defun egg:configure-machine ()
  (interactive)
  (egg:--core/configure-machine egg:this-machine))

(defmacro egg:defmachine! (machine id &rest body)
  (declare (indent 1))
  (let ((id-function (or (plist-get body :id-function) #'egg:identify-by-hostname))
	(init (plist-get body :init)))
    `(push
      (quote (,machine . (:init
			  (lambda () ,init)
			  :id ,id
			  :id-function ,id-function)))
      egg:--machines)))

(defvar egg:module-dir (file-name-concat user-emacs-directory "modules")
  "Eggmacs module base directory")

(defvar egg:modules '()
  "List of modules to load")

(defvar egg:module-defs '()
  "Module definitions")

(defun egg:--core/module-id (module)
  (if (consp module)
      (car module)
    module))

(defun egg:--core/load-module (module)
  (message "Loading %s" (symbol-name (egg:--core/module-id module)))
  (load
   (file-name-concat egg:module-dir
		     (symbol-name
		      (if (consp module)
			  (car module)
			module)))))

(defun egg:--core/load-modules (modules)
  (mapc #'egg:--core/load-module modules))

(defmacro egg:use-modules! (&rest modules)
  `(setq egg:modules (append egg:modules (quote ,modules))))

(defmacro egg:use-feature! (module-id &rest features)
  (let ((module `(assq (quote ,module-id) egg:modules)))
    `(when ,module (setcdr ,module (append (cdr ,module) (quote ,features))))))

(defmacro egg:defmodule! (module-id &rest body)
  (declare (indent 1))
  `(progn
     (if (alist-get (quote ,module-id) egg:module-defs)
	 (message "Module %s has already been defined" (symbol-name (quote ,module-id)))
       (push (cons (quote ,module-id) '()) egg:module-defs)
       (let* ((egg:--current-module (quote ,module-id))
              (egg:--module (assq egg:--current-module egg:modules))
              (egg:--features (when (consp egg:--module)
                                (cdr egg:--module))))
	 ,@body))))

(defmacro egg:initmodule! (&rest body)
  (declare (indent 0))
  `(add-hook 'after-init-hook
	     (lambda () ,@body)))

(defmacro egg:feature! (feature &rest body)
  (declare (indent 1))
  `(when (member (quote ,feature) egg:--features)
     ,@body))

(defmacro egg:defun! (function &rest body)
  (declare (indent 2))
  `(egg:--define! function ,function (defun ,function ,@body)))

(defmacro egg:defvar! (variable &rest body)
  (declare (indent 2))
  `(egg:--define! variable ,variable (defvar ,variable ,@body)))

(defmacro egg:--define! (sym-type sym &rest body)
  (declare (indent 1))
  (let ((module-def '(assq egg:--current-module egg:module-defs)))
    `(progn
       (push (cons (quote ,sym) (quote ,sym-type)) (cdr ,module-def))
       ,@body)))

(defmacro egg:hook! (hook &rest body)
  (declare (indent 1))
  `(add-hook (quote ,hook ) (lambda () ,@body)))

(defmacro egg:global-keys! (&rest bindings)
  (cons 'progn
	(mapcar
	 (lambda (binding)
	   (let ((key (car binding))
		 (bind (cdr binding)))
	     (list 'keymap-global-set key bind)))
	 bindings)))

(defmacro egg:keys! (keymap &rest bindings)
  (cons 'progn
	(mapcar
	 (lambda (binding)
	   (let ((key (car binding))
		 (bind (cdr binding)))
	     (list 'keymap-set keymap key bind)))
	 bindings)))

(defun egg:init ()
  (let* ((machine (egg:--core/identify-this-machine))
	 (machine-id (car machine))
	 (machine-config (cdr machine)))
    (setq egg:this-machine machine-id
	  egg:this-config machine-config)
    (egg:--core/configure-machine machine-config)
    (egg:--core/load-modules egg:modules)))
