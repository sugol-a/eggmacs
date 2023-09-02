(egg:defmodule! splash
  (egg:defvar! egg:splash/buffer-name "*eggmacs*")

  (egg:defvar! egg:splash/image
    (file-name-concat user-emacs-directory "/splash/splash.png"))

  (egg:defvar! egg:splash/slogans
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
      "Surprisingly legal!"
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
      "Partake in civil disobedience!"
      "Isn't vim!"))

  (egg:defvar! egg:--splash/image nil)
  (egg:defvar! egg:--splash/subtitle nil)
  (egg:defvar! egg:--splash/subtitle-position nil)
  (egg:defvar! egg:splash/writeout-delay 0.01)
  (egg:defvar! egg:splash/writeout-startup-delay 1.5)
  (egg:defvar! egg:--splash-writeout-timer nil)
  (egg:defvar! egg:--splash-writeout-counter nil))

;; (defvar egg:--splash-image nil)

;; (defvar egg:--subtitle nil)
;; (defvar egg:--subtitle-position nil)

;; (defvar egg:splash-writeout-delay 0.02)
;; (defvar egg:splash-writeout-startup-delay 1.8)
;; (defvar egg:--splash-writeout-timer nil)
;; (defvar egg:--splash-writeout-counter 0)

;; (defface egg:splash-title-face
;;   `((nil . (:height 200))
;;     (nil . (:weight 'bold)))
;;   "Splash title face")

;; (defface egg:splash-subtitle-face
;;   `((nil . (:inherit default :italic t :height 100)))
;;   "Splash subtitle face")

;; (defun egg:--random-slogan ()
;;   (let ((n (random (length egg:splash-slogans))))
;;     (nth n egg:splash-slogans)))

;; (defun egg:--splash-set-fringe ()
;;   (when egg:--splash-image
;;     (let* ((image-width (car (image-size egg:--splash-image t)))
;; 	   (window-width (window-pixel-width))
;; 	   (fringe (max 0 (floor (/ (- window-width image-width) 2)))))
;;       (set-window-fringes nil fringe fringe))))

;; (defun egg:--split-words (string max-length)
;;   (let ((tokens (s-split " " string)))
;;     (seq-reduce
;;      (lambda (accumulator token)
;;        (let* ((last-token (car (reverse accumulator)))
;; 	      (last-length (length last-token))
;; 	      (token-length (length token)))
;; 	 (flatten-list
;; 	  (if (and last-token (< (+ last-length token-length) max-length))
;; 	      (cons (reverse (cdr (reverse accumulator))) (s-join " " (list last-token token)))
;; 	    (cons accumulator token)))))
;;      tokens
;;      '())))

;; (defun egg:--splash-update-subtitle ()
;;   (setq-local buffer-read-only nil)
;;   (goto-char egg:--subtitle-position)

;;   (save-excursion
;;     (delete-region (point) (point-max)))

;;   (let* ((max-width (- (window-max-chars-per-line nil 'egg:splash-subtitle-face) 1))
;; 	 (lines (egg:--split-words (concat "― " (substring egg:--subtitle 0 egg:--splash-writeout-counter)) max-width)))
;;     (dolist (line lines)
;;       (let* ((pad (- max-width (length line)))
;; 	     (padded-line (concat (s-repeat pad " ")
;; 				  line)))
;; 	(insert (propertize padded-line 'face 'egg:splash-subtitle-face))
;; 	(newline))))

;;   (setq-local buffer-read-only t))

;; (defun egg:splash-refresh-subtitle ()
;;   (interactive)
;;   (setq egg:--subtitle (egg:--random-slogan))
;;   (egg:--splash-update-subtitle))

;; (defun egg:--splash-writeout-tick ()
;;   (if (>= egg:--splash-writeout-counter (length egg:--subtitle))
;;       (cancel-timer egg:--splash-writeout-timer)
;;     (setq-local egg:--splash-writeout-counter (+ 1 egg:--splash-writeout-counter))
;;     (setq-local egg:--splash-writeout-timer (run-with-timer egg:splash-writeout-delay nil #'egg:--splash-writeout-tick))
;;     (egg:--splash-update-subtitle))
;;   t)

;; (defun egg:--splash-begin-writeout ()
;;   (setq egg:--splash-writeout-timer (run-with-timer egg:splash-writeout-delay nil #'egg:--splash-writeout-tick)))

;; (defun egg:--splash-animate-image ()
;;   (image-animate egg:--splash-image))

;; (define-derived-mode egg:splash-mode
;;   fundamental-mode
;;   "Splash"
;;   "Splashscreen mode for egg."
;;   :interactive nil

;;   (newline 4)
;;   (let ((image (or egg:--splash-image (create-image egg:splash-image))))
;;     (insert-image image)
;;     (setq egg:--splash-image image))

;;   (newline)
;;   (setq-local egg:--subtitle-position (point))
;;   (egg:splash-refresh-subtitle)

;;   (local-set-key "q" (lambda ()
;; 		       (interactive)
;; 		       (kill-buffer)))
    
;;   (setq-local buffer-read-only t
;; 	      cursor-type nil)

;;   (add-hook 'window-configuration-change-hook (lambda ()
;; 						(egg:--splash-set-fringe)
;; 						(egg:--splash-update-subtitle)) 0 t)

;;   (when egg:splash-writeout-startup-delay
;;     (run-with-timer egg:splash-writeout-startup-delay nil #'egg:--splash-begin-writeout))
  
;;   (when (image-multi-frame-p egg:--splash-image)
;;     (add-hook 'window-setup-hook (lambda ()
;; 				   (if egg:splash-animation-delay
;; 				       (run-with-timer egg:splash-animation-delay
;; 						       nil
;; 						       #'egg:--splash-animate-image)
;; 				     (egg:--splash-animate-image))) 0 t)))

;; (defun egg:make-splash-buffer ()
;;   (interactive)
;;   (let ((splash-buffer (get-buffer-create egg:splash-buffer-name)))
;;     (with-current-buffer splash-buffer
;;       (egg:splash-mode))
;;     splash-buffer))

;; (egg:module! splash
;;   (egg:with-parameter! buffer-name
;;     (setq egg:splash-buffer-name buffer-name))

;;   (egg:with-parameter! image
;;     (setq egg:splash-image image))

;;   (egg:with-parameter! animation-delay
;;     (setq egg:splash-animation-delay animation-delay))

;;   (setq initial-buffer-choice #'egg:make-splash-buffer))
