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
  :bind
  ("C-c e" . consult-flymake)
  :custom
  (flymake-diagnostic-functions nil)
  (flymake-show-diagnostics-at-end-of-line 'short)
  :config
  (defun thy/flymake-diagnostic-oneliner (diag &optional nopaintp)
    "Get truncated one-line text string for diagnostic DIAG.
This is useful for displaying the DIAG's text to the user in
confined spaces, such as the echo are.  Unless NOPAINTP is t,
propertize returned text with the `echo-face' property of DIAG's
type."
    (let* ((txt (car (split-string (flymake-diagnostic-text diag) "\\:")))
           (txt (substring txt 0 (cl-loop for i from 0 for a across txt
                                          when (eq a ?\n) return i))))
      (if nopaintp txt
        (propertize txt 'face
                    (flymake--lookup-type-property
                      (flymake-diagnostic-type diag) 'echo-face 'flymake-error)))))
  (advice-add #'flymake-diagnostic-oneliner :override #'thy/flymake-diagnostic-oneliner))
