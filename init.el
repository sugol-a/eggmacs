(setq user-full-name "Alister Sanders"
      user-mail-address "alister@sugol.org"
      custom-file "/dev/null"
      backup-inhibited t)

;; ----------------------------------------
;;          d e v e l o p m e n t
;; ----------------------------------------
(defun al:prog-mode ()
  (keymap-local-set "M-RET" #'egg:extend-comment)
  (keymap-local-set "C-c M-q" #'al:reflow-line)
  (display-line-numbers-mode +1))

(add-hook 'prog-mode-hook #'al:prog-mode)

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

(defun al:c-mode ()
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (c-set-offset 'access-label '-)
  (c-set-offset 'inclass #'al:c-lineup)
  (indent-tabs-mode -1)
  (keymap-local-set "C-c M-;" #'al:c-insert-doc-comment))

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

(add-hook 'c-mode-common-hook #'al:c-mode)

(defun al:lsp-mode ()
  (keymap-local-set "C-c l" (define-keymap
			      "r" #'lsp-rename
			      "f" #'lsp-format-buffer)))

(add-hook 'lsp-mode-hook #'al:lsp-mode)

(setq-default display-line-numbers-width 4)

;; ----------------------------------------
;;             o r g - m o d e
;; ----------------------------------------

;; Stolen from Doom
(define-minor-mode al:org-pretty-mode
  "Hide emphasis markers and toggles pretty entities"
  :init-value nil
  :lighter " *"
  (setq org-hide-emphasis-markers al:org-pretty-mode)
  (org-toggle-pretty-entities)
  (with-silent-modifications
    (org-table-map-tables 'org-table-align t)))

(egg:package! org-fragtog
  :defer t
  :hook (org-mode . org-fragtog-mode)
  :commands (org-fragtog-mode))

(egg:package! org-drill
  :defer t
  :commands (org-drill))

(egg:package! visual-fill-column
  :defer t
  :commands (visual-fill-column-mode))

(defface al:org-subtle
  `((t :inherit default :foreground "#707070" :weight light))
  "Face for subtle elements")

(defun al:org-mode ()
  (org-indent-mode +1)
  (al:org-pretty-mode +1)

  (setq visual-fill-column-width 100
	fill-column 80)

  (setq-local visual-fill-column-center-text t)
  (visual-fill-column-mode +1)

  (setq-local face-remapping-alist
	      (let* ((background (face-attribute 'default :background))
		     (face `(:inherit al:org-subtle :box (:color ,background :line-width (12 . 12) :style flat))))
		`((header-line . ,face)
		  (mode-line . ,face)
		  (mode-line-active . ,face))))

  (al:org-update-header)
  (al:org-update-mode-line)
  (add-hook 'after-change-functions #'al:org-buffer-change-hook 0 t)
  (add-hook 'window-configuration-change-hook #'al:org-update-header 0 t)
  (add-hook 'window-configuration-change-hook #'al:org-update-mode-line 0 t)
  (add-hook 'after-save-hook #'al:org-update-mode-line))

(defun al:org-update-header ()
  (let* ((count (count-words (point-min) (point-max)))
	 (word-count-string (format "%d words" count)))
    (setq header-line-format (format (format "%%%ds" (- (window-total-width) (length word-count-string))) word-count-string))))

(defun al:org-update-mode-line ()
  (setq-local mode-line-format
	      `("%b"
		;;mode-line-buffer-identification
		,(if (buffer-modified-p)
		     " [*]"
		   ""))))

(defun al:org-buffer-change-hook (beg end len)
  (al:org-update-header)
  (al:org-update-mode-line))

(setq org-babel-load-languages (cl-map 'list (lambda (lang) `(,lang . t)) '(dot emacs-lisp python))

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

(add-hook 'org-mode-hook #'al:org-mode)

;; ----------------------------------------
;;               v i s u a l
;; ----------------------------------------

(defvar al:vfc-text-scale 1.5
  "Text scale for visual-fill-column-mode")

(define-minor-mode al:visual-fill-column-mode
  "Customized visual-fill-column-mode"
  :init-value nil
  :lighter "VF"
  (if al:visual-fill-column-mode
      (progn
	(setq al:--vfc-restore-line-numbers display-line-numbers-mode
	      al:--vfc-restore-text-scale text-scale-mode-amount)
	
	(setq-local visual-fill-column-center-text t)
	
	(visual-fill-column-mode 1)
	(display-line-numbers-mode -1)
	(text-scale-set al:vfc-text-scale))
    (progn
      (when al:--vfc-restore-line-numbers
	(display-line-numbers-mode 1))
      
      (when al:--vfc-restore-text-scale
	(text-scale-set al:--vfc-restore-text-scale))
      
      (visual-fill-column-mode -1))))

;; Global user interface keybinds
(keymap-global-set "C-c t" (define-keymap
			     "z" #'al:visual-fill-column-mode))

(egg:package! doom-themes
  :config (setq doom-themes-enable-bold t
		doom-themes-enable-italic t))

;; ----------------------------------------
;;               e d i t o r
;; ----------------------------------------

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

(keymap-global-set "C-c d" #'al:duplicate-line)

;; ----------------------------------------
;;                   🥚
;; ----------------------------------------

(setq egg:modules
      `((ui
	 :features (+vertico +marginalia +treemacs +popwin +zoom +which-key)
	 :theme doom-1337
	 :font ,(font-spec :family "IBM Plex Mono" :size 16 :weight 'regular)
	 :variable-pitch-font ,(font-spec :family "IBM Plex Sans" :size 16 :weight 'light)
	 :smooth-scroll t
	 :keys (:treemacs
		(("C-c C-SPC" . treemacs)
		 ("C-c o p" . treemacs-select-window)))

	 :zoom (:size (0.66 . 0.66))
	 :which-key (:delay 0.1))
	
	(dev
	 :features (+projectile +company +flycheck +lsp +lsp-ui +rust +meson)
	 :keys (:projectile
		(("C-c p" . projectile-command-map)))
	 :lsp (:hooks (c-mode-hook))
	 :lsp-ui (:disable-features (doc-hover-mouse))
	 :company (:delay 0.1 :min-prefix 2))

	(edit
	 :features (+expand +mc +smartparens +snippet +bol-or-text +focus)
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
			     "b" #'sp-backward-sexp)))))))

(egg:init)
