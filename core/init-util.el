;;; -*- lexical-binding: t -*-

;; Useful functions and macros shared by multiple modules.
(require 'cl-lib)

;; Keep generated files out of the main configuration.
(use-package no-littering
  :ensure t
  ;; Later modules call its path helpers while they are being loaded.
  :demand t
  :config
  (setq custom-file (no-littering-expand-var-file-name "custom.el")))

;; Obsidian loads Elgrep, but this configuration does not persist its UI state.
(use-package elgrep
  :ensure nil
  :custom
  (elgrep-data-file nil))

;; Show color previews in markup and style buffers.
(use-package rainbow-mode
  :ensure t
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode))

(use-package transient
  :ensure nil
  ;; Later modules expand Transient macros while they are being loaded.
  :demand t)
