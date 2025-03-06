;;; -*- lexical-binding: t -*-

;; comint 是一个与交互式进程通信的包，这似乎是为了 compile 做准备的
(use-package comint
  :straight nil
  :config
  (setq comint-prompt-read-only t ; 设置交互进程为只读模式
        comint-buffer-maximum-size 2048)) ; 设置交互进程大小

;; compile 是内置的配合 make 的包
(use-package compile
  :straight t
  :config
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)

  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  )

;; 快速编译运行当前 buffer, 刷算法题和写脚本应该很舒服
(use-package quickrun
  :straight t
  :bind (("C-c x"  . quickrun)))

;; 语法检查
;; flymake-show-buffer-diagnostics 可以显示错误并复制
;; 实际上用 embark 然后 w 也可以
(use-package flymake
  :straight t
  :hook
  ((prog-mode . flymake-mode))
  :bind
  ("C-c e" . consult-flymake)
  :config
  (setq flymake-diagnostic-functions nil)
  (setq flymake-show-diagnostics-at-end-of-line 'short)
  )
