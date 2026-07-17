;; -*- lexical-binding: t; -*-

(use-package rime
  :ensure t
  :demand t
  :preface
  (defun thy/rime-finalize ()
    "Finalize librime before Emacs unloads its dynamic module."
    (when (and (bound-and-true-p rime--lib-loaded)
               (fboundp 'rime-lib-finalize))
      (rime-lib-finalize)
      (setq rime--lib-loaded nil)))

  (defun thy/rime-install-customization ()
    "Install the tracked Rime customization in the user data directory."
    (let* ((source (expand-file-name "etc/rime/rime_frost.custom.yaml"
                                     user-emacs-directory))
           (directory (no-littering-expand-var-file-name "rime/"))
           (destination (expand-file-name "rime_frost.custom.yaml" directory)))
      (when (or (not (file-exists-p destination))
                (file-newer-than-file-p source destination))
        (make-directory directory t)
        (copy-file source destination t t))))
  :init
  (thy/rime-install-customization)
  :custom
  (default-input-method "rime")
  (rime-disable-predicates '(rime-predicate-evil-mode-p))
  (rime-emacs-module-header-root
   (expand-file-name "../Resources/include/" invocation-directory))
  (rime-librime-root
   (cond
    ((file-directory-p "/opt/homebrew/opt/librime/")
     "/opt/homebrew/opt/librime/")
    ((file-directory-p "/usr/local/opt/librime/")
     "/usr/local/opt/librime/")))
  (rime-show-candidate 'posframe)
  (rime-show-preedit 'inline)
  (rime-share-data-dir
   (expand-file-name "~/projects/dotfiles/rime/rime-frost/"))
  (rime-user-data-dir (no-littering-expand-var-file-name "rime/"))
  :config
  (add-hook 'kill-emacs-hook #'thy/rime-finalize))

(use-package sis
  :ensure t
  :after rime
  :demand t
  :bind ("<f18>" . thy/sis-switch)
  :preface
  (defun thy/sis-switch ()
    "Switch input source unless Evil is in a non-input state."
    (interactive)
    (if (rime-predicate-evil-mode-p)
        (sis-set-english)
      (sis-switch)))

  (defun thy/sis-update-other-cursor-color (&rest _)
    "Use the current theme's error color for non-English input."
    (setq sis-other-cursor-color
          (or (face-foreground 'error nil t) "red"))
    (when (and (bound-and-true-p sis-global-cursor-color-mode)
               (fboundp 'sis--update-cursor-color))
      (sis--update-cursor-color)))
  :config
  (sis-ism-lazyman-config nil "rime" 'native)
  (thy/sis-update-other-cursor-color)
  (add-hook 'enable-theme-functions #'thy/sis-update-other-cursor-color)
  (sis-global-cursor-color-mode 1)
  (sis-global-respect-mode 1)
  (sis-global-context-mode 1)
  (sis-global-inline-mode 1))
