(defvar native-comp-deferred-compilation-deny-list nil)

(setq user-full-name "Alister Sanders"
      user-mail-address "alister@sugol.org"
      custom-file "/dev/null"
      backup-inhibited t)

(egg:package! benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; ----------------------------------------
;;          d e v e l o p m e n t
;; ----------------------------------------
(egg:extend-mode! prog-mode-hook
  (progn (keymap-local-set "M-RET" #'egg:extend-comment)
	 (keymap-local-set "C-c M-q" #'al:reflow-line)
	 (display-line-numbers-mode +1))
  :hook t)

(setq lsp-keymap-prefix "C-c l")

(defun al:--iimage-insert (filename)
  (interactive)
  (let ((filename (file-relative-name (expand-file-name filename))))
    (if (egg:inside-comment-p)
	(insert (format "<%s>" filename)))
      (indent-according-to-mode)
      (insert
       (comment-padright
	comment-start)
       (format "<%s>" filename))
      (save-excursion
	(unless (string= "" comment-end)
	  (insert (comment-padleft comment-end)))))
    (al:iimage-redisplay))

(defun al:iimage-insert ()
  (interactive)
  (when-let* ((image-filename (read-file-name "Insert image ")))
    (al:--iimage-insert image-filename)))

(defvar al:iimage-edit-command #'al:iimage-edit-krita-flatpak)

(defun al:iimage-edit-krita-flatpak (image-filename)
  (eq 0 (call-process "flatpak"
		      nil
		      nil
		      nil
		      "run"
		      "org.kde.krita"
		      "--canvasonly"
		      "--nosplash"
		      image-filename)))

(defun al:--iimage-make-canvas (scratch-filename)
  (when-let ((size (read-from-minibuffer "Size? ")))
    (let ((background (car (alist-get 'bg doom-themes--colors))))
      (call-process "convert"
		    nil
		    nil
		    nil
		    "-size"
		    size
		    (format "xc:%s" background)
		    scratch-filename))
    t))

(defun al:iimage-redisplay ()
  (interactive)
  (iimage-mode-buffer nil)
  (iimage-mode-buffer t))

(defun al:iimage-create-and-insert ()
  (interactive)
  (let ((image-scratch-filename (expand-file-name
				 (concat (make-temp-name "eggmacs-scratch-") ".png")
				 user-emacs-directory)))
      (when (al:--iimage-make-canvas image-scratch-filename)
	(funcall al:iimage-edit-command image-scratch-filename)
	(if-let ((save-filename (read-file-name "Save to ")))
	    (progn
	      (rename-file image-scratch-filename save-filename 1)
	      (when (yes-or-no-p "Insert file? ")
		(message "Inserting %s" save-filename)
		(al:--iimage-insert save-filename)))
	  (delete-file image-scratch-filename)))))

(defun al:c-insert-doc-comment ()
  (interactive)
  (let ((column (- (point)
		   (save-excursion
		     (beginning-of-line)
		     (point)))))
    (insert "/**")
    (newline)
    (indent-to column)
    (insert " * ")
    (newline)
    (indent-to column)
    (insert " */")
    (previous-line)))

(defun al:c-lineup (langelem)
  (let ((inclass (assoc 'inclass c-syntactic-context)))
    (save-excursion
      (c-beginning-of-defun)
      (if (or (looking-at "struct")
	      (looking-at "typedef struct")
	      (looking-at "union")
	      (looking-at "typedef union"))
	  '+
	'++))))

(defun al:reflow-line ()
  (interactive)
  (let ((begin (save-excursion
		 (beginning-of-line)
		 (point)))
	(end (save-excursion
	       (end-of-line)
	       (point))))
    (save-restriction
      (narrow-to-region begin end)
      (prog-fill-reindent-defun))))

(egg:extend-mode! 
  c-mode-common-hook
  (progn (setq c-basic-offset 4)
	 (c-set-style "bsd")
	 (setq c-basic-offset 4)
	 (c-set-offset 'case-label '+)
	 (c-set-offset 'access-label '-)
	 (c-set-offset 'inclass #'al:c-lineup)
	 (indent-tabs-mode -1)
	   
	 (keymap-local-set "C-c M-;" #'al:c-insert-doc-comment))
  :hook t)

(egg:extend-mode! prog-mode-hook
  (progn
    (iimage-mode +1))
  :hook t)

(egg:package! svelte-mode)

(egg:package! lice
  :config
  (setq lice:default-license "mit"))

(setq-default display-line-numbers-width 4)

(setq js-jsx-syntax t)

(egg:extend-mode! js-ts-mode-hook
  (progn
    (indent-tabs-mode -1)
    (setq-local tab-width 2
		js-indent-level 2
		js-switch-indent-offset 2))
  
  :hook t)

(egg:extend-mode! emacs-lisp-mode-hook
  (progn
    (company-mode))
  :hook t)

(defun al:typescript-indent-setup ()
  (indent-tabs-mode -1)
  (setq-local tab-width 4
	      js-indent-level 4
	      typescript-ts-mode-indent-offset 4))

(egg:extend-mode! typescript-ts-mode-hook
  (al:typescript-indent-setup)
  :hook t)

(egg:extend-mode! tsx-ts-mode-hook
  (al:typescript-indent-setup)
  :hook t)

(defun al:insert-snippet (name)
  (insert name)
  (yas-expand))

(defun al:insert-default-license ()
  (interactive)
  (lice lice:default-license))

(defun al:auto-insert-main-c ()
  (al:insert-default-license)
  (newline)
  (insert "#include <stdio.h>\n"
	  "#include <stdlib.h>\n")
  (newline)
  (al:insert-snippet "main"))

(defun al:auto-insert-header-c ()
  (al:insert-default-license)
  (newline)
  (al:insert-snippet "once"))

(defun al:auto-insert-main-c++ ()
  (al:insert-default-license)
  (newline)
  (insert "#include <iostream>\n")
  (newline)
  (al:insert-snippet "main"))

(defun al:auto-insert-header-c++ ()
  (al:insert-default-license)
  (newline)
  (al:insert-snippet "guard")
  (yas-next-field)
  (yas-exit-all-snippets)
  (al:insert-snippet "cls11"))

(defun al:auto-insert-meson ()
  (let ((project-name (or (projectile-project-name)
			  (file-name-base (file-name-directory (buffer-file-name)))
			  "project-name")))
    (save-excursion
      (al:insert-default-license)
      (newline)
      (insert (format "project('%s', 'c',\n" project-name)
	      (format "        version: '0.1',\n")
	      (format "        default_options: ['warning_level=3'])\n"))
      (newline)
      (insert "pkg = import('pkgconfig')\n\n"
	      "deps = [  ]\n\n"
	      "inc = include_directories('.')\n\n"
	      "sources = [  ]\n\n"
	      (format "exe = executable('%s',\n" project-name)
	      (format "                 sources: sources,\n")
	      (format "                 include_directories: inc,\n")
	      (format "                 dependencies: deps)")))))

(defun al:auto-insert-main-rust ()
  (al:insert-default-license)
  (newline)
  (al:insert-snippet "main"))

;; ----------------------------------------
;;             o r g - m o d e
;; ----------------------------------------

(egg:extend-mode! org-mode
  (progn
    ;; (setq-local org-hide-emphasis-markers t
    ;; 		fill-column 80
    ;; 		face-remapping-alist (let* ((background (face-attribute 'default :background))
    ;; 					    (face `(:inherit al:org-subtle :box (:color ,background :line-width (12 . 12) :style flat))))
    ;; 				       `((header-line . ,face)
    ;; 					 (mode-line . ,face)
    ;; 					 (mode-line-active . ,face))))
    (org-toggle-pretty-entities)
    (org-indent-mode +1)
    ;; (al:org-update-header)
    ;; (al:org-update-mode-line)
    
    (with-silent-modifications
      (org-table-map-tables 'org-table-align t))

    ;; (add-hook 'after-change-functions #'al:org-buffer-change-hook 0 t)
    ;; (add-hook 'window-configuration-change-hook #'al:org-update-header 0 t)
    ;; (add-hook 'window-configuration-change-hook #'al:org-update-mode-line 0 t)
    ;; (add-hook 'after-save-hook #'al:org-update-mode-line)
    ))

(egg:package! org-fragtog
  :defer t
  :hook (org-mode . org-fragtog-mode)
  :commands (org-fragtog-mode))

(defvar al:org-babel-load-languages
  '(dot emacs-lisp python))

(setq org-babel-load-languages (cl-map 'list (lambda (lang) `(,lang . t)) al:org-babel-load-languages)
      ;; Allows us to underline an entire heading using
      org-fontify-whole-heading-line t

      ;; Hide everything on startup
      org-startup-folded t

      org-image-actual-width 600)

(custom-set-faces
 '(org-level-1 ((t (:inherit default :underline t :height 1.4))))
 '(org-level-2 ((t (:inherit default :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :height 1.15))))
 '(org-level-4 ((t (:inherit default :weight bold))))
 '(org-level-5 ((t (:inherit default :weight bold))))
 '(org-level-6 ((t (:inherit default :weight bold))))
 '(org-level-7 ((t (:inherit default :weight bold))))
 '(org-level-8 ((t (:inherit default :weight bold)))))

;; ----------------------------------------
;;               v i s u a l
;; ----------------------------------------

(egg:package! doom-themes
  :config (setq doom-themes-enable-bold t
		doom-themes-enable-italic t))

(egg:package! base16-theme)

(defvar al:mode-line-padding '(1 . 8))

(defun al:configure-mode-line ()
  (let ((mode-line-bg (car (alist-get 'mode-line doom-themes--colors)))
	(mode-line-inactive-bg (car (alist-get 'mode-line-inactive doom-themes--colors))))
    (set-face-attribute 'mode-line nil
			:box `(:line-width ,al:mode-line-padding :color ,mode-line-bg :style flat-button))
    (set-face-attribute 'mode-line-inactive nil
			:box `(:line-width ,al:mode-line-padding :color ,mode-line-inactive-bg :style flat-button))))

(add-hook 'after-init-hook #'al:configure-mode-line)

(setq frame-resize-pixelwise t)

(global-hl-line-mode 1)

;; ----------------------------------------
;;               e d i t o r
;; ----------------------------------------
(defun al:line-extents ()
  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))
      (end-of-line)
      (let ((eol (point)))
	`(,bol . ,eol)))))

(defun al:duplicate-line (&optional arg)
  (interactive "p")
  (let ((beg-end (save-excursion
		   (beginning-of-line)
		   (let ((bol (point)))
		     (end-of-line)
		     (let ((eol (point)))
		       `(,bol . ,eol))))))
    (kill-ring-save (car beg-end) (cdr beg-end))

    (dotimes (n (or arg 1))
      (save-excursion
	(end-of-line)
	(newline)
	(beginning-of-line)
	(yank)))
    ;; don't pollute the kill-ring
    (when kill-ring
      (setq kill-ring (cdr kill-ring)))))

(defun al:move-line-up ()
  (interactive)
  (let* ((line-extents (al:line-extents))
	 (bol (car line-extents))
	 (eol (cdr line-extents))
	 (current-point (point))
	 (column (- current-point bol)))
    (kill-whole-line)
    (previous-line)
    (yank)
    (previous-line)
    (beginning-of-line)
    (forward-char column)
    (when kill-ring
      (setq kill-ring (cdr kill-ring)))))

(defun al:move-line-down ()
  (interactive)
  (let* ((line-extents (al:line-extents))
	 (bol (car line-extents))
	 (eol (cdr line-extents))
	 (current-point (point))
	 (column (- current-point bol)))
    (kill-whole-line)
    (next-line)
    (yank)
    (previous-line)
    (beginning-of-line)
    (forward-char column)
    (when kill-ring
      (setq kill-ring (cdr kill-ring)))))

(egg:package! orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(global-subword-mode 1)

;; ----------------------------------------
;;                   🥚
;; ----------------------------------------

(egg:define-machines!
 ("Alisters-iMac.local" .
  (:init
   (progn
     (setq ns-command-modifier 'meta
	   ns-alternate-modifier 'super)

     (setq lsp-clients-clangd-executable "/usr/local/Cellar/llvm/16.0.6/bin/clangd")

     (egg:define-keys! ()
       (:global
	("<end>" . #'move-end-of-line)
	("<home>" . #'egg:beginning-of-line-or-text)))
     
     ;; (add-to-list 'default-frame-alist '(fullscreen . fullscreen))
     )
   :vars
   `(:font ,(font-spec :family "SF Mono" :size 16 :weight 'light))))

 ("Alisters-MacBook-Pro.local" .
  (:init
   (progn
     (setq ns-command-modifier 'meta
	   ns-alternate-modifier 'super)

     (setq lsp-clients-clangd-executable "/usr/local/Cellar/llvm/16.0.6/bin/clangd")

     (egg:define-keys! ()
       (:global
	("<end>" . #'move-end-of-line)
	("<home>" . #'egg:beginning-of-line-or-text)))
     
     (add-to-list 'default-frame-alist '(fullscreen . fullscreen)))
   :vars
   `(:font ,(font-spec :family "Space Mono" :size 16 :weight 'light))))

 ("kronos" .
  (:init nil
   :vars
   `(:font ,(font-spec :family "SF Mono" :size 16 :weight 'normal)))))

(egg:define-keys!
    ((al:æsthetic-map . "Commands that vaguely change the look of a frame/window/buffer")
     (al:lsp-map . "LSP Commands"))
  (:global
   ("M-o" . #'other-window)
   ("M-2" . #'split-window-vertically)
   ("M-3" . #'split-window-horizontally)
   ("M-0" . #'delete-window))

  (:global
   ("M-<up>" . #'al:move-line-up)
   ("M-<down>" . #'al:move-line-down)
   ("C-c d" . #'al:duplicate-line))

  (:global
   ("C-c DEL" . #'egg:delete-to-end-of-previous-line))

  (:global
   ("C-c C-SPC" . #'treemacs)
   ("C-c o p" . #'treemacs-select-window))

  (:hook
   lsp-mode-hook
   ("C-c l" . al:lsp-map))

  (:hook
   lsp-mode-hook
   ("M-." . #'lsp-find-definition))

  (:keymap
   al:lsp-map
   ("r" . #'lsp-rename)
   ("f" . #'lsp-format-buffer)
   ("a" . #'lsp-execute-code-action))

  (:global
   ("C-c h" . al:æsthetic-map))

  (:keymap
   al:æsthetic-map
   ("z" . #'egg:glasses-mode)
   ("f" . #'toggle-frame-fullscreen)))

(setq egg:modules
      `((ui
	 :features (+vertico +marginalia +treemacs +popwin +zoom +which-key +mood-line)
	 :theme base16-black-metal-bathory
	 :font ,(egg:machine-var! :font)
	 :variable-pitch-font ,(font-spec :family "IBM Plex Sans" :size 16 :weight 'light)
	 :smooth-scroll t
	 :keys (:treemacs
		(;; ("C-c C-SPC" . treemacs)
		 ("C-c o p" . treemacs-select-window)))

	 :zoom (:size (0.66 . 0.66))
	 :which-key (:delay 0.1))
	
	(dev
	 :features (+projectile
		    +company
		    +flycheck
		    ;; +rainbow-delim
		    +lsp
		    +lsp-ui
		    +treesit
		    +rust
		    +meson
		    +python
		    +pyright
		    +pipenv
		    +pyvenv
		    +typescript)
	 :keys (:projectile
		(("C-c p" . projectile-command-map)))
	 :lsp (:hooks (c-mode-hook python-mode-hook js-mode-hook js-ts-mode-hook jsx-mode-hook c++-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
	 :lsp-ui (:disable-features (doc-hover-mouse))
	 :company (:delay 0.1 :min-prefix 2))

	(edit
	 :features (+expand +mc +smartparens +snippet +bol-or-text +focus +autoinsert)
	 :keys (:expand
		(("C-=" . er/expand-region))
		:mc
		(("C-c m" . ,(define-keymap
			       "l" #'mc/edit-lines
			       "n" #'mc/insert-numbers
			       "c" #'mc/insert-letters)))
		:smartparens
		(("M-p" . ,(define-keymap
			     "u" #'sp-unwrap-sexp
			     "f" #'sp-forward-sexp
			     "b" #'sp-backward-sexp
			     "(" #'sp-wrap-round
			     "[" #'sp-wrap-square
			     "{" #'sp-wrap-curly))))
	 :autoinsert (:templates (("^main\\.c$" . al:auto-insert-main-c)
				  ("\\.c$" . al:insert-default-license)
				  ("\\.h$" . al:auto-insert-header-c)
				  ("^main\\.cpp$" . al:auto-insert-main-c++)
				  ("\\.cpp$" . al:insert-default-license)
				  ("\\.hpp$" . al:auto-insert-header-c++)
				  ("^meson\\.build" . al:auto-insert-meson)
				  ("^main\\.rs" . al:auto-insert-main-rust))))

	(splash :animation-delay 0.8)
	(glasses)))

(egg:init)
