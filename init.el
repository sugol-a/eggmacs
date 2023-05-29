(defvar native-comp-deferred-compilation-deny-list nil)

(setq user-full-name "Alister Sanders"
      user-mail-address "alister@sugol.org"
      custom-file "/dev/null"
      backup-inhibited t)

;; ----------------------------------------
;;          d e v e l o p m e n t
;; ----------------------------------------
(egg:extend-mode! prog-mode-hook
  (progn (keymap-local-set "M-RET" #'egg:extend-comment)
	 (keymap-local-set "C-c M-q" #'al:reflow-line)
	 (display-line-numbers-mode +1))
  :hook t)

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

(egg:extend-mode! lsp-mode-hook
  (keymap-local-set "C-c l" (define-keymap
			      "r" #'lsp-rename
			      "f" #'lsp-format-buffer
			      "a" #'lsp-execute-code-action))
  :hook t)

(egg:package! svelte-mode)

(egg:package! lice
  :config
  (setq lice:default-license "mit"))

(setq-default display-line-numbers-width 4)

(setq js-jsx-syntax t)

(egg:extend-mode! js-mode-hook
  (progn
    (indent-tabs-mode -1)
    (setq-local tab-width 2
		js-indent-level 2
		js-switch-indent-offset 2)
    (keymap-local-set "M-." #'lsp-find-definition))
  
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
    (setq-local org-hide-emphasis-markers t
		fill-column 80
		face-remapping-alist (let* ((background (face-attribute 'default :background))
					    (face `(:inherit al:org-subtle :box (:color ,background :line-width (12 . 12) :style flat))))
				       `((header-line . ,face)
					 (mode-line . ,face)
					 (mode-line-active . ,face))))
    (org-toggle-pretty-entities)
    (org-indent-mode +1)
    (al:org-update-header)
    (al:org-update-mode-line)
    
    (with-silent-modifications
      (org-table-map-tables 'org-table-align t))

    (add-hook 'after-change-functions #'al:org-buffer-change-hook 0 t)
    (add-hook 'window-configuration-change-hook #'al:org-update-header 0 t)
    (add-hook 'window-configuration-change-hook #'al:org-update-mode-line 0 t)
    (add-hook 'after-save-hook #'al:org-update-mode-line)))

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

(defvar al:glasses-fringe-width 340
  "Fringe width (pixels) to add when al:glasses-mode is enabled.")

(defvar al:glasses-text-scale 0.8
  "Amount to scale text by when al:glasses-mode is enabled.")

(defvar al:glasses-line-spacing 0.1
  "Line spacing when al:glasses-mode is enabled.")

(defvar al:glasses-display-line-numbers nil
  "Set to non-nil to enable line numbers in al:glasses-mode, nil otherwise")

(defvar al:glasses-truncate-lines t
  "Set to non-nil to enable line truncation in al:glasses-mode, nil otherwise")

(defvar al:glasses-center-point t
  "Set to non-nil to keep point centered at all times when al:glasses-mode is enabled.")

(define-minor-mode al:glasses-mode
  "Glasses."
  :init-value nil
  :lighter "👓"
  (let ((inhibit-message t))
    (if al:glasses-mode
	(progn
	  (egg:stash 'al:glasses
		     `(set-window-fringes . ,(cons nil (window-fringes)))
		     `(text-scale-set . ,(list text-scale-mode-amount))
		     `(set-window-configuration . ,(list (current-window-configuration)))
		     `(toggle-truncate-lines . ,(list truncate-lines))
		     'line-spacing
		     'display-line-numbers
		     'scroll-margin
		     'maximum-scroll-margin
		     'mode-line-format)
	  (set-window-fringes nil
			      al:glasses-fringe-width
			      al:glasses-fringe-width)
	  (text-scale-set al:glasses-text-scale)
	  (delete-other-windows)
	  (toggle-truncate-lines al:glasses-truncate-lines)
	  (setq display-line-numbers al:glasses-display-line-numbers
		line-spacing al:glasses-line-spacing)

	  (when al:glasses-center-point
	    (setq-local scroll-margin most-positive-fixnum
			maximum-scroll-margin 0.5)))

      (egg:unstash 'al:glasses))))

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

(egg:machines!
 (("Alisters-iMac.local" . (progn
				(setq ns-command-modifier 'meta
				      ns-alternate-modifier 'super)
                                (keymap-global-set "<end>" #'move-end-of-line)
                                (keymap-global-set "<home>" #'egg:beginning-of-line-or-text)
				(add-to-list 'default-frame-alist '(fullscreen . fullscreen))))))

;; ----------------------------------------
;;                   🥚
;; ----------------------------------------

(egg:define-keys! ((al:æsthetic-map . "Commands that vaguely change the look of a frame/window/buffer"))
  (:global
   ("M-o" . #'other-window)
   ("M-2" . #'split-window-vertically)
   ("M-3" . #'mouse-split-window-horizontally)
   ("M-0" . #'delete-window))

  (:global
   ("M-<up>" . #'al:move-line-up)
   ("M-<down>" . #'al:move-line-down)
   ("C-c d" . #'al:duplicate-line))

  (:global
   ("C-c C-SPC" . #'treemacs)
   ("C-c o p" . #'treemacs-select-window))

  (:global
   ("C-c h" . al:æsthetic-map))

  (:keymap
   al:æsthetic-map
   ("z" . #'al:glasses-mode)))

(setq egg:modules
      `((ui
	 :features (+vertico +marginalia +treemacs +popwin +zoom +which-key +mood-line)
	 :theme doom-moonlight
	 :font ,(font-spec :family "SF Mono" :size 16 :weight 'light)
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
	 :lsp (:hooks (c-mode-hook python-mode-hook js-mode-hook jsx-mode-hook c++-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
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
	(splash :animation-delay 0.8)))

(egg:init)
