;;; -*- lexical-binding: t -*-

;; Useful functions and macros shared by multiple modules.
(require 'cl-lib)

;; Keep generated files out of the main configuration.
(use-package no-littering
  :ensure t
  :demand t
  :config
  (setq custom-file (no-littering-expand-var-file-name "custom.el"))
  (load custom-file 'noerror 'nomessage))

;; Show color previews in markup and style buffers.
(use-package rainbow-mode
  :ensure t
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode))

(use-package transient
  :ensure t
  :demand t)
