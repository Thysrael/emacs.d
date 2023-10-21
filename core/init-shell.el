(use-package vterm
  :bind
  (("C-t" . +start-vterm-in-project)
   :map vterm-mode-map
        ("C-y" . vterm-yank)
        ("M-y" . vterm-yank-pop))
  :init
  (setq vterm-shell "zsh")
  :config
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
