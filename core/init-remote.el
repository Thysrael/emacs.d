;; [tramp] Edit file remotely
(use-package tramp
  :straight nil
  :config
  ;; 似乎是支持 remote 启动其他进程的
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)

  (setq tramp-default-method "ssh"
        tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosaves/")
        tramp-backup-directory-alist backup-directory-alist
        remote-file-name-inhibit-cache 60 ; 加速，允许 cache
        remote-file-name-inhibit-locks t ; 加速，不会使用文件锁
        tramp-verbose 0 ; 加速，更少的 tramp 信息
        vc-handled-backends '(SVN Git) ; 加速，禁用一些版本控制后端
        )
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=no -t")
  ;; 只需要输入一次密码 https://www.reddit.com/r/emacs/comments/3liwm7/is_it_possible_to_configure_tramp_such_that_i/
  (setq tramp-use-ssh-controlmaster-options nil)
  )

;; (use-package docker
;;   :straight t
;;   :bind ("C-c d" . docker))
