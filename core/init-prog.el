;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :straight nil
    :hook ((prog-mode . prettify-symbols-mode)))

;; comint 是一个与交互式进程通信的包，如 shell 等
(use-package comint
  :straight nil
  :config
  (setq comint-prompt-read-only t ; 设置交互进程为只读模式
        comint-buffer-maximum-size 2048)) ; 设置交互进程大小
