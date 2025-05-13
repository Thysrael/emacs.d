;; -*- lexical-binding: t; -*-

;; [tramp] Edit file remotely
(use-package tramp
  :config
  ;; 似乎是支持 remote 启动其他进程的
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  ;; 如果设置了这条命令，就会导致 ssh 无法使用 eglot, 只能用 sshx
  ;; 而如果使用 sshx ，又无法使用 dirvish 的预览功能，乐
  ;; ssh 在远程机器上打开一个正常的交互 shell ，而 sshx 使用 `ssh -t -t host -l user /bin/sh` 来打开连接
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  ;; (add-to-list 'tramp-remote-path "/home/qs/.local/bin")
  ;; 我目前的理解是 tramp-own-remote-path 表示的是用 user 在登录后使用的环境变量
  ;; 而原本的 tramp-remote-path 是没有登录后的变量的
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; 加速，更少的 tramp 信息
  (setq tramp-verbose 0)
  :custom
  ;; 禁用保存，提高速度
  (remote-file-name-inhibit-auto-save t)
  (tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosaves/"))
  (tramp-backup-directory-alist backup-directory-alist)
  ;; 禁用 shell history 文件
  (shell-history-file-name t)
  ;; tramp 的默认方法
  (tramp-default-method "ssh")
  ;; 加速，允许 cache 在 60s 内使用
  (remote-file-name-inhibit-cache 60)
  ;; 加速，不会使用文件锁
  (remote-file-name-inhibit-locks t)
  ;; 加速，禁用一些版本控制后端
  (vc-handled-backends '(Git))
  ;; (vc-handled-backends nil)
  ;; 只需要输入一次密码 https://www.reddit.com/r/emacs/comments/3liwm7/is_it_possible_to_configure_tramp_such_that_i/
  (tramp-use-ssh-controlmaster-options nil)
  (tramp-chunksize 2000)
  )

;; (use-package docker
;;   :straight t
;;   :bind ("C-c d" . docker))
