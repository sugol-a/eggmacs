;;; -*- lexical-binding: t; -*-
(egg:defmodule! org
  (egg:initmodule!
    (egg:feature! +clean
      (egg:defun! egg:--org-clean-font-lock-keywords ()
        (let* ((left (or (car (window-margins)) 0))
               (margin-format (format "%%%ds" left)))
          `(
            ("^\\(\\*\\{1\\} \\)\\(.*\\)$"
             1 '(face shadow display ((margin left-margin)
                                      ,(propertize (format margin-format "* ")
                                                   'face '(:inherit shadow :weight light)) append))
             2 '(face bold append))

            ("^\\(\\*\\{2\\} \\)\\(.*\\)$"
             1 '(face shadow display ((margin left-margin)
                                      ,(propertize (format margin-format "** ")
                                                   'face '(:inherit shadow :weight light)) append))
             2 '(face bold append))

            ("^\\(\\*\\{3\\} \\)\\(.*\\)$"
             1 '(face shadow display ((margin left-margin)
                                      ,(propertize (format margin-format "*** ")
                                                   'face '(:inherit shadow :weight light)) append))
             2 '(face bold append))

            ("^\\(\\*\\{4\\} \\)\\(.*\\)$"
             1 '(face shadow display ((margin left-margin)
                                      ,(propertize (format margin-format "**** ")
                                                   'face '(:inherit shadow :weight light)) append))
             2 '(face bold append)))))

      (egg:package! org-modern
        :defer t
        :config (setq org-modern-hide-stars nil
                      org-modern-star nil)
        :hook (org-mode . org-modern-mode))

      (egg:package! topspace
        :defer t
        :hook (org-mode . topspace-mode))

      (egg:package! visual-fill-column
        :defer t
        :commands (visual-fill-column-mode))

      ;; FIXME org-appear is broken by changes to org-mode 9.7, so we
      ;; have to pull in a specific branch with the fixes
      (egg:package! org-appear
        :straight (:host github :repo "awth13/org-appear" :branch "origin/org-9.7-fixes"))

      (setq org-hide-emphasis-markers t)

      (egg:defun! egg:--org-clean-mode-fontify ()
        (when (window-margins)
          (font-lock-add-keywords nil (egg:--org-clean-font-lock-keywords))
          (font-lock-fontify-buffer)
          (remove-hook 'window-configuration-change-hook #'egg:--org-clean-mode-fontify t)))

      (egg:defun! egg:org-enable-language (lang)
        (when (not (member lang (mapcar #'car org-babel-load-languages)))
          (push `(,lang . t) org-babel-load-languages)))

      (egg:defun! egg:org-disable-language (lang)
        (when (member lang (mapcar #'car org-babel-load-languages))
          ()))

      (define-minor-mode egg:org-clean-mode
        "Clean, sane defaults for org-mode"
        :init-value nil
        (if egg:org-clean-mode
              (progn
                (when (derived-mode-p 'org-mode)
                  (setq-local visual-fill-column-width 80
                              visual-fill-column-center-text t)
                  (add-to-list 'font-lock-extra-managed-props 'display)

                  (visual-fill-column-mode 1)
                  (visual-line-mode 1)
                  (org-appear-mode 1)
                  (set-window-buffer nil (window-buffer))
                  (add-hook 'window-configuration-change-hook
                            #'egg:--org-clean-mode-fontify
                            nil t)))
            (visual-fill-column-mode -1)
            (visual-line-mode -1))))))
