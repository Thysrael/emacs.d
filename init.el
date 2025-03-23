;; 一个启动优化，通过将 file-name-handler-alist 置 nil 来避免检查
;; magic file 机制
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

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
                     'init-modal
                     'init-export
                     'init-ai
                     ))

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
    (load-file (concat init-directory (symbol-name file) ".el"))))

;; 重新恢复 file-name-handler-alist
(setq file-name-handler-alist my-saved-file-name-handler-alist)
