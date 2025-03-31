;;; -*- lexical-binding: t -*-

;; 判断当前是否在服务器上
(defvar on-server
  (let ((hostname (system-name)))
    (not (or (string= hostname "banana")
             (string= hostname "pitch")
             (string= hostname "thysrael-82av")
             ))))

;; 根据是否在服务器上选择性加载文件
(defvar +init-files nil)
(if (not on-server) ; local
    (setq +init-files
          (list
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
  (setq +init-files ; remote
        (list
         'init-package
         'init-util
         'init-tmp
         'init-basic
         'init-enhance
         'init-window
         'init-tabbar
         'init-navigate
         'init-ui
         'init-modeline
         'init-edit
         'init-completion
         'init-prog
         'init-lang
         'init-project
         'init-vsc
         'init-debug
         'init-write
         'init-remote
         'init-dired
         'init-modal
         ))
  )

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file +init-files)
    (load-file (concat init-directory (symbol-name file) ".el"))))
