;;; -*- lexical-binding: t -*-

;;; 项目管理
(use-package project
  :bind
  ("C-x p" . hydra-project/body)
  :config
  (defhydra hydra-project (:color blue
                        :hint nil
                        :columns 5)
  "Project Commands"
  ("b" project-list-buffers "List Buffers" :color blue)
  ("x" project-execute-extended-command "Execute Extended Command" :color blue)
  ("r" project-query-replace-regexp "Query Replace Regexp" :color blue)
  ("G" project-or-external-find-regexp "Find Regexp (External)" :color blue)
  ("g" project-find-regexp "Find Regexp (Project)" :color blue)
  ("p" project-switch-project "Switch Project" :color blue)
  ("k" project-kill-buffers "Kill Buffers" :color blue)
  ("e" project-eshell "Eshell" :color blue)
  ("c" project-compile "Compile" :color blue)
  ("v" project-vc-dir "Version Control (VC) Dir" :color blue)
  ("D" project-dired "Dired" :color blue)
  ("d" project-find-dir "Find Directory" :color blue)
  ("s" project-shell "Shell" :color blue)
  ("S" project-switch-to-buffer "Switch to Buffer" :color blue)
  ("F" project-or-external-find-file "Find File (External)" :color blue)
  ("f" project-find-file "Find File" :color blue)
  ("&" project-async-shell-command "Async Shell Command" :color blue)
  ("!" project-shell-command "Shell Command" :color blue))
)

;;; 版本管理
;; git chrunk
(use-package diff-hl
  :defines desktop-minor-mode-table
  :hook
  ; 在不同模式下挂在不同的高亮
  (find-file    . diff-hl-mode)
  (vc-dir-mode  . diff-hl-dir-mode)
  (dired-mode   . diff-hl-dired-mode)
  ((focus-in . diff-hl-update-once))
  ; Fall back to the display margin since the fringe is unavailable in tty
  :hook
  ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) .
         (lambda ()
           (diff-hl-update-once)
           (unless (display-graphic-p) (diff-hl-margin-local-mode 1))))
  :config
  (setq
   diff-hl-draw-borders nil
   ;; Reduce load on remote
   diff-hl-disable-on-remote t
   ;; A slightly faster algorithm for diffing
   vc-git-diff-switches '("--histogram"))

  ;; The gutter looks less cramped with some space between it and  buffer.
  (setq-default fringes-outside-margins t)

  ;; Make fringes look better
  (setq diff-hl-fringe-bmp-function
        (lambda (&rest _)
          (define-fringe-bitmap 'my-diff-hl-bmp
            (vector #b00000000)
            1 8
            '(center t))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Integration with flymake
  (with-eval-after-load 'flymake
    (setq flymake-fringe-indicator-position 'right-fringe))

  ;; WORKAROUND: Integration with ws-butler
  (advice-add #'ws-butler-after-save :after #'diff-hl-update-once)
  :bind
  ("C-c g" . diff-hl-show-hunk)
  )

;;; 编译运行
;; comint 是一个与交互式进程通信的包，这似乎是为了 compile 做准备的
(use-package comint
  :straight nil
  :config
  (setq comint-prompt-read-only t ; 设置交互进程为只读模式
        comint-buffer-maximum-size 2048)) ; 设置交互进程大小

;; compile 是内置的配合 make 的包
(use-package compile
  :config
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)

  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  )

;; 编译结果查看器（看上去像汇编和源码的对应关系）
(use-package rmsbolt
  :straight t)

;; 快速编译运行当前 buffer, 刷算法题和写脚本应该很舒服
(use-package quickrun
  :bind (("C-c x"  . quickrun)))

;; [flymake] On-the-fly syntax checker
(use-package flymake
  :hook
  ((prog-mode . flymake-mode))
  :bind
  ("C-c e" . consult-flymake)
  :config
  (setq
   flymake-diagnostic-functions nil)
  )
