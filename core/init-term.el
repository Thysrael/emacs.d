(use-package vterm
  :straight t
  :bind
  ;; 用 vterm-copy-mode 可以复制 vterm 输出
  (("C-t" . +start-vterm-in-project)
   :map vterm-mode-map
   ("C-y" . vterm-yank)
   ("M-y" . vterm-yank-pop)
   :map vterm-copy-mode)
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

;; 多 vterm 支持的同时不影响 vterm toggle 逻辑
(use-package multi-vterm
  :straight t
  :bind
  ("C-c t" . multi-vterm))
