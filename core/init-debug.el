;;; -*- lexical-binding: t -*-

(use-package comint
  :ensure nil
  :custom
  (comint-buffer-maximum-size 2048)
  (comint-prompt-read-only t))

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error)
  :config
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(use-package quickrun
  :ensure t
  :bind (("C-c x"  . quickrun)))

(use-package flymake
  :ensure nil
  :hook
  ((prog-mode . flymake-mode))
  :custom
  (flymake-diagnostic-functions nil)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-suppress-zero-counters nil))

(use-package consult-flymake
  :ensure nil
  :bind ("C-c e" . consult-flymake))
