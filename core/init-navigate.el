;;; -*- lexical-binding: t -*-

;; 改变光标移动绑定，使其更加方便
(global-set-key (kbd "C-f") '+smart-forward)
(defun +smart-forward ()
  "Move cursor based on its position in a word."
  (interactive)
  (cond
   ((looking-at "\\w") (forward-word 1))
   ((looking-at "\\s-") (progn (re-search-forward "\\S-") (backward-char)))
   (t (forward-char))))

;; (global-set-key (kbd "C-b") 'backward-word)

;; (defun +smart-backword ()
;;   "Move cursor based on its position in a word."
;;   (interactive)
;;   (cond
;;     ((looking-at "\\w") (backward-word)) ; 如果在单词中部，移动到单词头部
;;     ((looking-back "\\b" 1) (backward-word) (forward-word)) ; 如果在单词头部，移动到前一个单词的尾部
;;     ((looking-at "\\s-") (backward-word))
;;     (t (backward-char)))) ; 如果不在单词中，显示消息

;; (global-set-key (kbd "C-l") 'forward-char)
;; (global-set-key (kbd "C-h") 'backward-char)

;; 字符跳转
(use-package avy
  :straight t
  :bind
  ("C-h" . avy-goto-char-2)
  :config
  (setq avy-timeout-seconds 0.3) ; 0.3 秒后如果不连续击键，那么进入选择阶段
  (setq avy-background t) ; 在跳转时背景变黑
  :init
  (setq avy-single-candidate-jump nil)
  ;; (advice-add 'avy-action-goto :after (lambda (&rest _args)
  ;;                                       (forward-word)))
  )

(use-package ace-pinyin
  :straight t
  :after avy
  :init (ace-pinyin-global-mode t))

;; 增强 C-e 使得其可以在关键位置进行循环移动
(use-package mosey
  :straight t
  :bind
  ("C-e" . mosey-forward-cycle))

;; [beginend] Better M-< M-> for programming
(use-package beginend
  :straight t
  :hook (after-init . beginend-global-mode))

;; 结构化跳转
(use-package imenu
  :straight nil
  :hook ((prog-mode conf-mode yaml-mode markdown-mode org-mode) . imenu-add-menubar-index)
  :config
  (setq imenu-auto-rescan t)
  )

;; 中文分词跳转
(use-package cns
  :load-path "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/"
  :hook
  (org-mode . cns-mode)
  (gfm-mode . cns-mode)
  (eww-mode . cns-mode)
  :bind
  (:map cns-mode-map
   ("C-f" . cns-forward-word))
  :init
  (setq cns-prog "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws")
  (setq cns-dict-directory "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict")
  )
