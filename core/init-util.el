;;; -*- lexical-binding: t -*-

;;; Utilities

;; Keep generated files out of the main configuration.
(use-package no-littering
  :ensure t
  :config
  ;; Ignore Customize output instead of writing custom-set-* forms anywhere.
  (setq custom-file null-device))

;; Show color previews in markup and style buffers.
(use-package rainbow-mode
  :ensure t
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode))

;; Use a dedicated mono font where mixed-width text matters.
(defun thy/set-buffer-face-mode-mono ()
  "Use Sarasa Mono SC in the current buffer."
  (interactive)
  (setq-local buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode))
