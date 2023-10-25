;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :straight nil
  :hook ((prog-mode . prettify-symbols-mode)))


;; Cross reference, 是 emacs 的代码分析工具前端
;; 后端可以是基于 ctag 这种比较 fuzzy 的或者是基于 elgot 这种语义的
;; M-, 是回到上个位置（经常跳转，很容易乱）
;; M-. 找到定义点
;; M-? 进行 grep 搜索，是最没有办法的办法
;; 可以用 xref-backend-functions 查看后端函数
(use-package xref
  :config
  (setq
   xref-search-program 'ripgrep ; 设置工具为 riggrep
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-history-storage 'xref-window-local-history)

  (defadvice! +xref--push-marker-stack-a (&rest rest)
    :before '(find-function consult-imenu consult-ripgrep citre-jump)
    (xref-push-marker-stack (point-marker))) ; 这里是一个压栈函数，用于记录光标位置
  :bind
  ("M-/" . xref-find-apropos) ; 将搜索按钮放到更合适的地方
  )

;; 基于正则匹配的后端
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate) ; 将后端设置为 dumpjump
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'completing-read
        dumb-jump-aggressive t
        )
  )

;; [citre] Ctags-infra
;; peek 是一个很舒服的前端
;; M-n, M-p 可以在小窗里移动，M-N, M-P 在多个搜索源中移动
;; M-p l 具有 chain 式查看功能，可以在小窗里嵌套调用
(use-package citre
  :bind
  ;; (:map prog-mode-map
  ;;   ("C-t j" . +citre-jump)
  ;;   ("C-t k" . +citre-jump-back)
  ;;   ("C-t p" . citre-peek)
  ;;   ("C-t h" . citre-ace-peek)
  ;;   ("C-t u" . citre-update-this-tags-file)
  ;;   ("C-t n" . citre-create-tags-file))
  ("M-t" . hydra-citre/body)
  :init
  (require 'citre-config)
  :config
  (setq citre-auto-enable-citre-mode-modes '(prog-mode)
        ;; citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-enable-capf-integration t)

  (defun +citre-jump ()
    "Jump to the definition of the symbol at point. Fallback to `xref-find-definitions'."
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (defun +citre-jump-back ()
    "Go back to the position before last `citre-jump'. Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error (call-interactively #'xref-go-back))))

  ;; Use Citre xref backend as a [fallback]
  (defadvice! +citre--xref-fallback-a (fn &rest args)
    :around #'xref--create-fetcher
    (let ((fetcher (apply fn args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (ignore xref-backend-functions)
             (apply fn args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))
  (defhydra hydra-citre (:color blue)
    "Citre Commands"
    ("p" citre-peek "Peek")
    ("h" citre-ace-peek "Ace Peek")
    ("u" citre-update-this-tags-file "Update Tags File")
    ("n" citre-create-tags-file "Create Tags File"))
  )

(use-package eglot
  :bind
  ("C-c l" . hydra-eglot/body)
  :config
  (setq eglot-events-buffer-size 0
        eglot-connect-timeout 10
        eglot-autoshutdown t
        ;; use global completion styles
        completion-category-defaults nil)
  (defhydra hydra-eglot (:color blue :columns 3)
    "Eglot Menu"
    ("f" eglot-format "Format selection")
    ("b" eglot-format-buffer "Format buffer")
    ("r" eglot-rename "Rename symbol at point")
    ("d" eglot-find-declaration "Find declaration")
    ("i" eglot-find-implementation "Find implementation")
    ("t" eglot-find-typeDefinition "Find type definition")
    ("s" eglot-shutdown "Shutdown eglot server")
    ("c" consult-eglot-symbols "Consult Eglot Symbols")
    ("q" nil "Quit" :color blue))
  )

;; [consult-eglot] Eglot support for consult
;; C-M-., rember it!
(use-package consult-eglot
  :after consult eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)
              ("C-c n" . consult-eglot-symbols)))
