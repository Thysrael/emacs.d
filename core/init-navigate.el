;;; -*- lexical-binding: t -*-

;; Use a context-aware forward motion for convenient cursor movement.
(global-set-key (kbd "C-f") #'thy/smart-forward)
(defun thy/forward-word (&optional arg)
  "Move forward ARG words using the active `forward-word' remapping."
  (funcall (or (command-remapping 'forward-word) #'forward-word) (or arg 1)))

(defun thy/smart-forward ()
  "Move cursor based on its position in a word."
  (interactive)
  (cond
   ((looking-at "\\w") (thy/forward-word 1))
   ((looking-at-p "\\s-") (progn (re-search-forward "\\S-") (backward-char)))
   (t (forward-char))))

(defun thy/emt-evil-forward-word (orig-fun count)
  "Call ORIG-FUN with COUNT unless active EMT word motion succeeds."
  (if (and (bound-and-true-p emt-mode)
           (fboundp 'emt-forward-word))
      (condition-case nil
          (if (emt-forward-word count)
              0
            (funcall orig-fun count))
        (error (funcall orig-fun count)))
    (funcall orig-fun count)))

;; 字符跳转
(use-package avy
  :ensure t
  :bind
  ("C-h" . avy-goto-char-2)
  :config
  (setq avy-timeout-seconds 0.3) ; 0.3 秒后如果不连续击键，那么进入选择阶段
  (setq avy-background t) ; 在跳转时背景变黑
  )

;; 增强 C-e 使得其可以在关键位置进行循环移动
(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; 结构化跳转
(use-package imenu
  :ensure nil
  :hook ((prog-mode conf-mode yaml-mode markdown-mode markdown-ts-mode org-mode) . imenu-add-menubar-index)
  :config
  (setq imenu-auto-rescan t)
  )

;; 中文分词跳转
(use-package emt
  :if (eq system-type 'darwin)
  :vc (emt :url "https://github.com/roife/emt.git"
            :rev "master")
  :commands (emt-mode emt-forward-word emt-backward-word emt-download-module)
  :hook (after-init . emt-mode)
  :config
  (with-eval-after-load 'evil
    (advice-add #'evil--forward-word-respect-categories
                :around #'thy/emt-evil-forward-word)))
