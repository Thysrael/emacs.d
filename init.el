;; 测量启动时间
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.4f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time) ; 加入钩子

(defvar +init-files (list
                      'init-tmp
                      'init-package
                      'init-basic
                      'init-enhance
                      'init-ui
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
      (load-file (concat init-directory (symbol-name file) ".el"))))