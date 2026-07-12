;;; -*- lexical-binding: t -*-

(use-package prog-mode
  :ensure nil
  :hook
  ((prog-mode . prettify-symbols-mode))
  :custom
  (prettify-symbols-unprettify-at-point t))

;; Cross reference, 是 emacs 的代码分析工具前端
;; 后端可以是基于 ctag 这种比较 fuzzy 的或者是基于 elgot 这种语义的
;; M-, 是回到上个位置（经常跳转，很容易乱）
;; M-. 找到定义点
;; M-? 进行 grep 搜索，是最没有办法的办法
;; 可以用 xref-backend-functions 查看后端函数
(use-package xref
  :ensure nil
  :preface
  (defun thy/xref-push-marker-stack (&rest _)
    "Push the current position to `xref' marker stack."
    (xref-push-marker-stack (point-marker)))
  :config
  (setq
   xref-search-program 'ripgrep ; 设置工具为 riggrep
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-history-storage 'xref-window-local-history)

  ;; 这里是一个压栈函数，用于记录光标位置
  (dolist (command '(find-function consult-imenu consult-ripgrep citre-jump))
    (advice-add command :before #'thy/xref-push-marker-stack))

  ;; (defadvice! +xref--push-marker-stack-a (&rest rest)
  ;;   :before '(find-function consult-imenu consult-ripgrep citre-jump find-definition-with-lsp-bridge)
  ;;   (xref-push-marker-stack (point-marker))) ; 这里是一个压栈函数，用于记录光标位置
  ;;
  ;; (defun find-definitions-with-lsp-bridge ()
  ;;   (interactive)
  ;;   (if lsp-bridge-mode (lsp-bridge-find-def)
  ;;     (call-interactively 'xref-find-definitions))) ; 和 lsp-bridge-mode 结合
  ;; (global-set-key (kbd "M-.") #'find-definitions-with-lsp-bridge)
  :bind
  ("M-/" . xref-find-references); 将搜索按钮放到更合适的地方
  )

;; 基于正则匹配的后端
(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate) ; 将后端设置为 dumpjump
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'completing-read
        dumb-jump-aggressive t
        )
  )

(use-package eglot
  ;; :straight (eglot :type git
  ;;                 :host github
  ;;                 :repo "AkibAzmain/eglot"
  ;;                 :branch "semantic-tokens")
  :ensure nil
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-report-progress nil)
  ;; :custom-face
  ;; (eglot-highlight-symbol-face ((t (:inherit bold :family "JetBrainsMono"))))
  ;; :hook ((c-ts-mode c++-ts-mode) . eglot-ensure)
  :config
  (setq eglot-events-buffer-size 0
        eglot-connect-timeout 10
        eglot-autoshutdown t)
  ;; (transient-define-prefix transient-hydra ()
  ;;   ["Eglot Menu"
  ;;    [
  ;;     ("f" "Format selection" eglot-format)
  ;;     ("b" "Format buffer" eglot-format-buffer)
  ;;     ("r" "Rename symbol at point" eglot-rename)
  ;;     ]
  ;;    [
  ;;     ("d" "Find declaration" eglot-find-declaration)
  ;;     ("i" "Find implementation" eglot-find-implementation)
  ;;     ("t" "Find type definition" eglot-find-typeDefinition)
  ;;     ]
  ;;    [
  ;;     ("s" "Shutdown eglot server" eglot-shutdown)
  ;;     ("c" "Consult Eglot Symbols" consult-eglot-symbols)
  ;;     ]
  ;;    ]
  ;;   )
  )

;; consult-eglot-symbols 可以提供一个具有所有 lsp symbol 的候选栏
;; C-M-., rember it!
(use-package consult-eglot
  :ensure t
  :after consult eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)
              ))

;; 文档信息展示
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t)
  :bind
  ("M-;" . eldoc))


;; 新的 ts 语法高亮支持
(use-package treesit
  :ensure nil
  :demand t
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq major-mode-remap-alist
        (append '((c-mode . c-ts-mode)
                  (c++-mode . c++-ts-mode)
                  (cmake-mode . cmake-ts-mode)
                  (csharp-mode . csharp-ts-mode)
                  (dockerfile-mode . dockerfile-ts-mode)
                  (elixir-mode . elixir-ts-mode)
                  (go-mode . go-ts-mode)
                  (go-mod-mode . go-mod-ts-mode)
                  (go-work-mode . go-work-ts-mode)
                  (heex-mode . heex-ts-mode)
                  (java-mode . java-ts-mode)
                  (js-mode . js-ts-mode)
                  (javascript-mode . js-ts-mode)
                  (json-mode . json-ts-mode)
                  (lua-mode . lua-ts-mode)
                  (php-mode . php-ts-mode)
                  (python-mode . python-ts-mode)
                  (ruby-mode . ruby-ts-mode)
                  (rust-mode . rust-ts-mode)
                  (sh-mode . bash-ts-mode)
                  (typescript-mode . typescript-ts-mode)
                  (tsx-mode . tsx-ts-mode)
                  (css-mode . css-ts-mode)
                  (html-mode . html-ts-mode)
                  (mhtml-mode . mhtml-ts-mode)
                  (markdown-mode . markdown-ts-mode)
                  (gfm-mode . markdown-ts-mode)
                  (toml-mode . toml-ts-mode)
                  (yaml-mode . yaml-ts-mode))
                major-mode-remap-alist))
  ;; 使得 org src 可以自动映射到 ts 模式
  (defun thy/remap-org-src-mode (mode)
    "Make `org-src-get-lang-mode' respect `major-mode-remap-alist'."
    (alist-get mode major-mode-remap-alist mode))
  (advice-add #'org-src-get-lang-mode :filter-return #'thy/remap-org-src-mode)
  )

;; (use-package stickyfunc-enhance
;;   :straight t
;;   :init
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   (require 'stickyfunc-enhance)
;;   (setq semantic-stickyfunc-sticky-classes '(function type variable))
;;   :hook
;;   (prog-mode . semantic-mode))
