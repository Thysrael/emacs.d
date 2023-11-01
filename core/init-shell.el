(use-package vterm
  :bind
  (("C-t" . +start-vterm-in-project)
   :map vterm-mode-map
        ("C-y" . vterm-yank)
        ("M-y" . vterm-yank-pop))
  :init
  (setq vterm-shell "zsh")
  :config
  (setq vterm-tramp-shells '(("docker" "/bin/bash")
                             ("ssh" "'/bin/bash'"))) ; 指定在 tramp 时使用的 shell
  ; /bin/bash 先加双引号，后加单引号可以解决 ssh tmux error 具体文档如下 https://github.com/akermu/emacs-libvterm/issues/569
  ;; (setq vterm-always-compile-module t)
  (defun +start-vterm-in-project ()
  "Start vterm in the current project if available, otherwise start a regular vterm."
  (interactive)
  (if (project-current)
      (let ((default-directory (project-root (project-current))))
        (vterm))
    (vterm)))
  )

;; (use-package vterm-toggle
;;   :bind
;;   (([f6] . vterm-toggle)
;;    :map vterm-mode-map
;;    ([f6] . vterm-toggle)
;;    ([(control return)] . vterm-toggle-insert-cd))
;;   :custom
;;   (vterm-toggle-scope "project")
;;   :config
;;   (setq vterm-toggle-cd-auto-create-buffer nil)
;;   )

;; 多 vterm 支持的同时不影响 vterm toggle 逻辑
(use-package multi-vterm
  :bind
  ("C-c t" . multi-vterm))
