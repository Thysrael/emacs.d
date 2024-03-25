(defvar +init-files (list
                     'init-package
                     'init-util
                     'init-tmp
                     'init-basic
                     'init-enhance
                     'init-window
                     ;; 'init-workspace
                     'init-tabbar
                     'init-navigate
                     'init-ui
                     'init-modeline
                     'init-input
                     'init-edit
                     'init-completion
                     'init-prog
                     'init-lang
                     'init-project
                     'init-vsc
                     'init-debug
                     'init-write
                     ;; 'init-shell
                     'init-term
                     'init-remote
                     'init-read
                     'init-dired
                     'init-control
                     'init-note
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
    (load-file (concat init-directory (symbol-name file) ".el"))))
