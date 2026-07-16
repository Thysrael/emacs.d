;;; -*- lexical-binding: t -*-

;; Use a context-aware forward motion for convenient cursor movement.
(use-package emacs
  :ensure nil
  :preface
  (defun thy/forward-word (&optional arg)
    "Move forward ARG words using the active `forward-word' remapping."
    (funcall (or (command-remapping 'forward-word) #'forward-word) (or arg 1)))

  (defun thy/smart-forward ()
    "Move cursor based on its position in a word."
    (interactive)
    (cond
     ((eobp) nil)
     ((looking-at "\\w") (thy/forward-word 1))
     ((looking-at-p "\\s-")
      (if (re-search-forward "\\S-" nil t)
          (backward-char)
        (goto-char (point-max))))
     (t (forward-char))))
  :bind ("C-f" . thy/smart-forward))

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
  :hook ((prog-mode conf-mode yaml-ts-mode markdown-mode markdown-ts-mode org-mode) . imenu-add-menubar-index)
  :custom
  (imenu-auto-rescan t))

;; 中文分词跳转
(use-package emt
  :if (eq system-type 'darwin)
  :vc (emt :url "https://github.com/roife/emt.git"
           :rev "master")
  :commands (emt-mode emt-forward-word emt-backward-word emt-download-module)
  :preface
  (defun thy/emt-evil-forward-word-begin (orig-fun count &optional bigword)
    "Use EMT token boundaries for CJK `evil-forward-word-begin' motions."
    (if (or bigword
            (evil-operator-state-p)
            (not (bound-and-true-p emt-mode))
            (not (fboundp 'emt-forward-word))
            (< (or count 1) 0))
        (funcall orig-fun count bigword)
      (dotimes (_ (or count 1))
        (if (and (char-after)
                 (eq (char-syntax (char-after)) ?w)
                 (looking-at-p "\\cc\\|\\cj\\|\\ch"))
            (let ((origin (point)))
              (if (condition-case nil
                      (and (emt-forward-word 1)
                           (/= (point) origin))
                    (error nil))
                  (when (looking-at-p "[[:space:]]")
                    (funcall orig-fun 1 nil))
                (goto-char origin)
                (funcall orig-fun 1 nil)))
          (funcall orig-fun 1 nil)))))
  :hook (after-init . emt-mode)
  :config
  (with-eval-after-load 'evil
    (advice-add #'evil-forward-word-begin
                :around #'thy/emt-evil-forward-word-begin)))
