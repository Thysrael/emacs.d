;; -*- lexical-binding: t; -*-

(use-package tramp
  :ensure nil
  :custom
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-cache 60)
  (remote-file-name-inhibit-locks t)
  (shell-history-file-name t)
  (tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosaves/"))
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-chunksize 2000)
  (tramp-default-method "ssh")
  (tramp-use-ssh-controlmaster-options nil)
  (tramp-verbose 0)
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  (add-to-list 'tramp-remote-path "~/.opencode/bin")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package tramp-rpc
  :after tramp
  :vc (:url "https://github.com/ArthurHeymans/emacs-tramp-rpc"
            :rev :newest
            :lisp-dir "lisp")
  :custom
  (tramp-rpc-deploy-local-cache-directory
   (no-littering-expand-var-file-name "tramp-rpc/"))
  (tramp-rpc-deploy-git-build-policy 'release))
