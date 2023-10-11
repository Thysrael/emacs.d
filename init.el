;; 测量启动时间
(defun efs/display-startup-time ()
  (interactive)
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.4f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(defvar +init-files (list
                      'init-package
                      'init-tmp
                      'init-basic
                      'init-enhance
                      'init-ui
                      'init-modeline
                      'init-input
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
      (load-file (concat init-directory (symbol-name file) ".el"))))