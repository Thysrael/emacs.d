;; [tramp] Edit file remotely
(use-package tramp
  :straight nil
  :config
  (setq tramp-default-method "ssh"
        tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosaves/")
        tramp-backup-directory-alist backup-directory-alist
        remote-file-name-inhibit-cache 60)
  )

(use-package docker
  :bind ("C-c d" . docker))
