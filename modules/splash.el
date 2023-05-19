(defvar egg:splash-buffer-name "*eggmacs*")

(defvar egg:splash-image (concat (expand-file-name user-emacs-directory)
				 "/splash/splash.png"))

(defvar egg:splash-slogans
  '("Potentially carcinogenic!"
    "Completely tax-exempt!"
    "Gluten free!"
    "Technically a religion!"
    "Contains asbestos!"
    "Also try dd if=/dev/urandom of=/dev/sda!"
    "Unorthodox, yet strangely effective!"
    "Indulge in organized chaos!"
    "Certifiably unpredictable!"
    "Safely untested!"
    "Shockingly legal!"
    "Quantum-level innovation!"
    "Exhilaratingly complex!"
    "Unravel the enigma of Emacs!"
    "Defies reason!"
    "Transcend coding paradigms through AI-powered algorithms!"
    "Enter the realm of hyperconverged code architecture!"
    "Deploy scalable solutions using cloud-native quantum algorithms!"
    "Master the art of neural network transmutation!"
    "Carbonated Water, Sucrose, Glucose, Acidity Regulators (Citric Acid, Sodium Bicarbonate, Magnesium Carbonate), Taurine (0.4%), Flavours, Colours (Caramel I, Riboflavin), Caffeine (0.03%), Vitamins (Niacinamide, Pantothenic Acid, B6, B12)"
    "The only cure for sadness!"
    "Your subscription will expire in 3 days!"
    "Partakes in civil disobedience!"
    "Isn't vim!"))

(defvar egg:--splash-image nil)

(defvar egg:--subtitle nil)
(defvar egg:--subtitle-position nil)

(defface egg:splash-title-face
  `((nil . (:height 200))
    (nil . (:weight 'bold)))
  "Splash title face")

(defface egg:splash-subtitle-face
  `((nil . (:inherit default :italic t :height 100)))
  "Splash subtitle face")

(defun egg:--random-slogan ()
  (let ((n (random (length egg:splash-slogans))))
    (nth n egg:splash-slogans)))

(defun egg:--splash-set-fringe ()
  (when egg:--splash-image
    (let* ((image-width (car (image-size egg:--splash-image t)))
	   (window-width (window-pixel-width))
	   (fringe (max 0 (floor (/ (- window-width image-width) 2)))))
      (set-window-fringes nil fringe fringe))))

(defun egg:--pad-string-centered (string width)
  (let* ((total-pad (- width (length string)))
	 (left-pad (ceiling (/ total-pad 2)))
	 (right-pad (- total-pad left-pad)))
    (concat (s-repeat left-pad " ")
	    string
	    (s-repeat right-pad " "))))

(defun egg:--split-words (string max-length)
  (let ((tokens (s-split " " string)))
    (seq-reduce
     (lambda (accumulator token)
       (let* ((last-token (car (reverse accumulator)))
	      (last-length (length last-token))
	      (token-length (length token)))
	 (flatten-list
	  (if (and last-token (< (+ last-length token-length) max-length))
	      (cons (reverse (cdr (reverse accumulator))) (s-join " " (list last-token token)))
	    (cons accumulator token)))))
     tokens
     '())))

(defun egg:--splash-update-subtitle ()
  (setq-local buffer-read-only nil)
  (goto-char egg:--subtitle-position)

  (save-excursion
    (delete-region (point) (point-max)))

  (let* ((max-width (- (window-max-chars-per-line nil 'egg:splash-subtitle-face) 1))
	 (lines (egg:--split-words (concat "― " egg:--subtitle) max-width)))
    (dolist (line lines)
      (let* ((pad (- max-width (length line)))
	     (padded-line (concat (s-repeat pad " ")
				  line)))
	(insert (propertize padded-line 'face 'egg:splash-subtitle-face))
	(newline))))

  (setq-local buffer-read-only t))

(defun egg:splash-refresh-subtitle ()
  (interactive)
  (setq egg:--subtitle (egg:--random-slogan))
  (egg:--splash-update-subtitle))

(define-derived-mode egg:splash-mode
  fundamental-mode
  "Splash"
  "Splashscreen mode for egg."
  :interactive nil

  (newline 4)
  (let ((image (or egg:--splash-image (create-image egg:splash-image))))
    (insert-image image)
    (setq egg:--splash-image image))

  (newline)
  (setq egg:--subtitle-position (point))
  (egg:splash-refresh-subtitle)

  (local-set-key "r" 'egg:splash-refresh-subtitle)

  (local-set-key "q" (lambda ()
		       (interactive)
		       (kill-buffer)))
    
  (setq-local buffer-read-only t
	      cursor-type nil)

  (add-hook 'window-configuration-change-hook (lambda ()
						(egg:--splash-set-fringe)
						(egg:--splash-update-subtitle)) 0 t))

(defun egg:make-splash-buffer ()
  (interactive)
  (let ((splash-buffer (get-buffer-create egg:splash-buffer-name)))
    (with-current-buffer splash-buffer
      (egg:splash-mode))
    splash-buffer))

(egg:module! splash
  (egg:with-parameter! buffer-name
    (setq egg:splash-buffer-name buffer-name))

  (egg:with-parameter! image
    (setq egg:splash-image image))

  (setq initial-buffer-choice #'egg:make-splash-buffer))
