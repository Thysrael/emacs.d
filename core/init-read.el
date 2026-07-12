;; -*- lexical-binding: t; -*-

(use-package eww
  :ensure nil
  :preface
  (defun thy/toggle-eww ()
    "Switch to an existing EWW buffer, or prompt for a URL."
    (interactive)
    (if-let* ((buffer (get-buffer "*eww*")))
        (switch-to-buffer buffer)
      (eww (read-string "Enter URL: "))))
  :hook
  (eww-mode . thy/setup-eww-buffer)
  :bind
  ("C-c r e" . thy/toggle-eww)
  :custom
  (eww-retrieve-command '("readable"))
  (shr-max-image-proportion 0.6)
  :config
  (defun thy/setup-eww-buffer ()
    "Apply buffer-local reading settings for EWW."
    (setq-local fill-column 140)
    (setq-local line-spacing 0.15)
    (setq-local scroll-margin 2)
    (setq-local truncate-lines nil)))

(use-package image-mode
  :ensure nil
  :bind
  (:map image-mode-map
        ("=" . image-increase-size)
        ("-" . image-decrease-size)))

(use-package doc-view
  :ensure nil
  :bind
  (:map doc-view-mode-map
        ("=" . doc-view-enlarge)
        ("-" . doc-view-shrink))
  :custom
  (doc-view-cache-directory (no-littering-expand-var-file-name "doc-view/"))
  (doc-view-resolution 200))
