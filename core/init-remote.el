;; [tramp] Edit file remotely
(use-package tramp
  :straight nil
  :config
  (setq tramp-default-method "ssh"
        tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosaves/")
        tramp-backup-directory-alist backup-directory-alist
        remote-file-name-inhibit-cache 60)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=no -t")
  ;; 只需要输入一次密码 https://www.reddit.com/r/emacs/comments/3liwm7/is_it_possible_to_configure_tramp_such_that_i/
  (setq tramp-use-ssh-controlmaster-options nil)
  )

(use-package docker
  :straight t
  :bind ("C-c d" . docker))
