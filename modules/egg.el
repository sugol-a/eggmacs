;;; -*- lexical-binding: t; -*-

(defvar egg:alert-buffer-name "🥚")

(defmacro egg:--format! (format-string args)
  `(format format-string ,@args))

(defun egg:--log (format-string &rest args)
  (let ((alert-buffer (get-buffer-create egg:alert-buffer-name)))
    (end-of-buffer)
    (insert (egg:--format! format-string args))
    (newline)))

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
  "List of loaded modules")

(defvar egg:module-defs '()
  "Module definitions")

(defun egg:load-module (module)
  (message "Loading %s" (symbol-name module))
  (when (load (file-name-concat egg:module-dir (symbol-name module)))
    (push module egg:modules)))

(defmacro egg:modules! (&rest modules)
  `(mapc #'egg:load-module (quote ,modules)))

(defmacro egg:defmodule! (module-id &rest body)
  (declare (indent 1))
  `(progn
     (if (alist-get (quote ,module-id) egg:module-defs)
	 (egg:--log "Module %s has already been defined" (symbol-name (quote ,module-id)))
       (push (cons ,module-id nil) egg:module-defs)
       (let ((egg:--current-module (quote ,module-id)))
	 ,@body))))

(defmacro egg:defun! (function &rest body)
  (declare (indent 2))
  `(egg:--define! func ,function (defun ,function ,@body)))

(defmacro egg:--define! (sym-type sym &rest body)
  (declare (indent 1))
  (let ((module-def '(assq egg:--current-module egg:module-defs)))
    `(progn
       (setcdr ,module-def (cons (cdr ,module-def) (cons ,sym ,sym-type)))
       ,@body)))

(defun egg:init ()
  (let* ((machine (egg:--core/identify-this-machine))
	 (machine-id (car machine))
	 (machine-config (cdr machine)))
    (setq egg:this-machine machine-id
	  egg:this-config machine-config)
    (egg:--core/configure-machine machine-config)))
