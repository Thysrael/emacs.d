;;; -*- lexical-binding: t -*-

;; 改变光标移动绑定，使其更加方便
(global-set-key (kbd "C-f") 'forward-word)
;; (global-set-key (kbd "C-b") 'backward-word)
;; (global-set-key (kbd "C-l") 'forward-char)
;; (global-set-key (kbd "C-h") 'backward-char)

;; 字符跳转
(use-package avy
  :bind
  ("C-h" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.3) ; 0.3 秒后如果不连续击键，那么进入选择阶段
  (setq avy-background t)) ; 在跳转时背景变黑

;; 增强 C-e 使得其可以在关键位置进行循环移动
(use-package mosey
  :bind
  ("C-e" . mosey-forward-cycle))

;; [beginend] Better M-< M-> for programming
(use-package beginend
  :hook (after-init . beginend-global-mode))

;; 结构化跳转
(use-package imenu
  :straight nil
  :hook ((prog-mode conf-mode yaml-mode markdown-mode org-mode) . imenu-add-menubar-index)
  :config
  (setq imenu-auto-rescan t)
  )
