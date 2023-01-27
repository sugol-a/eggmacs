(defmacro egg:module! (module &rest body)
  (declare (indent 1))
  `(let* ((egg:--current-module (quote ,module))
	  (egg:--current-module-parameters (alist-get (quote ,module) egg:modules))
	  (egg:--current-module-features (plist-get egg:--current-module-parameters :features))
	  (egg:--module-keys (plist-get egg:--current-module-parameters :keys)))
     ,@body))

(defun egg:--symbol-to-prop (symbol)
  (intern (concat ":" (symbol-name symbol))))

(defmacro egg:parameter! (parameter)
  (declare (indent 1))
  `(plist-get egg:--current-module-parameters (egg:--symbol-to-prop (quote ,parameter))))

(defmacro egg:with-parameter! (parameter &rest body)
  (declare (indent 1))
  `(progn
     (when-let ((,parameter (egg:parameter! ,parameter)))
       ,@body)))

(defmacro egg:bind-parameters! (feature &rest bindings)
  (declare (indent 1))
  `(egg:with-parameter! ,feature
     (progn
       ,@(cl-map 'list
		(lambda (binding)
		  `(when-let ((source (plist-get ,feature ,(car binding))))
		     (setq ,(cdr binding) source)))
		bindings))))

(defmacro egg:feature-gate! (features &rest body)
  (declare (indent 1))
  (if (consp features)
      `(when (cl-subsetp ,features egg:--current-module-features)
	,@body)
    `(when (member (quote ,features) egg:--current-module-features)
       ,@body)))

(defmacro egg:keys! (bindset)
  `(plist-get egg:--module-keys (egg:--symbol-to-prop (quote ,bindset))))

(defun egg:module-feature (module feature)
  (when-let ((module-features (egg:module-parameter module :features)))
    (member feature module-features)))

(defun egg:module-parameter (module parameter)
  (plist-get (alist-get module egg:modules) parameter))
