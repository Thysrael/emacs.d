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
  :straight t
  :config
  (setq
   xref-search-program 'ripgrep ; 设置工具为 riggrep
   xref-show-definitions-function #'xref-show-definitions-completing-read
   xref-show-xrefs-function #'xref-show-definitions-completing-read
   xref-history-storage 'xref-window-local-history)

  (defadvice! +xref--push-marker-stack-a (&rest rest)
    :before '(find-function consult-imenu consult-ripgrep citre-jump)
    (xref-push-marker-stack (point-marker))) ; 这里是一个压栈函数，用于记录光标位置

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
  :straight t
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
  :straight t
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
  ;; :straight (eglot :type git
  ;;                 :host github
  ;;                 :repo "AkibAzmain/eglot"
  ;;                 :branch "semantic-tokens")

  :straight t
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  ;; :hook ((c-ts-mode c++-ts-mode) . eglot-ensure)
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

;; consult-eglot-symbols 可以提供一个具有所有 lsp symbol 的候选栏
;; C-M-., rember it!
(use-package consult-eglot
  :straight t
  :after consult eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . consult-eglot-symbols)
              ))

;; 加速 eglot
;; 需要先安装 https://github.com/blahgeek/emacs-lsp-booster
;; 在使用 elgot 前先开启 eglot-booster
;; 不知道为什么必须手动开启
;; TODO: eglot-booster 无法放到 local 下，是因为 exec-path 的问题
(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :demand t
  :config (eglot-booster-mode)
  )

;; 让 eglot 可以丰富 tempel, 使用中老报 warning
;; (use-package eglot-tempel
;;   :straight t
;;   :after (eglot tempel)
;;   :init
;;   (eglot-tempel-mode)
;;   )


;; (use-package lsp-mode
;;   :straight t
;;   :custom-face
;;   (lsp-inlay-hint-face ((t (:inherit lsp-details-face))))
;;   :config
;;   (setq lsp-headerline-arrow ":")
;;   :custom
;;   (lsp-headerline-breadcrumb-icons-enable t)
;;   (lsp-headerline-breadcrumb-segments '(symbols))
;;   (lsp-eldoc-render-all t)
;;   :hook
;;   (lsp-mode . lsp-inlay-hints-mode))
;;
;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;   "Try to parse bytecode instead of json."
;;   (or
;;    (when (equal (following-char) ?#)
;;      (let ((bytecode (read (current-buffer))))
;;        (when (byte-code-function-p bytecode)
;;          (funcall bytecode))))
;;    (apply old-fn args)))
;; (advice-add (if (progn (require 'json)
;;                        (fboundp 'json-parse-buffer))
;;                 'json-parse-buffer
;;               'json-read)
;;             :around
;;             #'lsp-booster--advice-json-parse)
;;
;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;   "Prepend emacs-lsp-booster command to lsp CMD."
;;   (let ((orig-result (funcall old-fn cmd test?)))
;;     (if (and (not test?)                             ;; for check lsp-server-present?
;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;              lsp-use-plists
;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;              (executable-find "emacs-lsp-booster"))
;;         (progn
;;           (message "Using emacs-lsp-booster for %s!" orig-result)
;;           (cons "emacs-lsp-booster" orig-result))
;;       orig-result)))
;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
;;
;; (use-package lsp-imenu
;;   :init
;;   ;; 启用 lsp-imenu 集成
;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

;; (use-package lsp-ui
;;   :straight t
;;   :init
;;   ;; 启用 lsp-ui
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   ;; 启用 flycheck
;;   (add-hook 'python-mode-hook 'flycheck-mode))

;; 文档信息展示
(use-package eldoc
  :straight t
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t)
  :bind
  ("M-;" . eldoc))

;; 格式化文档注释自动生成
(use-package docstr
  :straight t
  ;; :straight
  ;; (docstr :type git :host github :repo "thysrael/docstr"
  ;;         :files (:defaults "langs/*.el"))
  :init
  (setq docstr-key-support t) ; 设置键入触发
  :config
  (docstr-faces-apply) ; 设置高亮
  ;; :hook
  ;; (after-init . global-docstr-mode)
  :custom
  (docstr-desc-summary "@summary ")
  (docstr-desc-param " ")
  (docstr-desc-return " ")
  )

;; 高亮数字
(use-package highlight-numbers
  :straight t
  :hook
  (prog-mode . highlight-numbers-mode)
  :custom-face
  (highlight-numbers-number ((t (:foreground "#8be9fd" :inherit font-lock-constant-face)))))

;; 新的 ts 语法高亮支持
;; (use-package treesit-auto
;;   :straight t
;;   :demand t
;;   :init
;;   (setq treesit-font-lock-level 4)
;;   (setq treesit-language-source-alist
;;         '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
;;           (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
;;           (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
;;           (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
;;           (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
;;           (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
;;           (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
;;           (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
;;           (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
;;           (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
;;           (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
;;           (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
;;           (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
;;           (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
;;           (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
;;           (make       . ("https://github.com/alemuller/tree-sitter-make"))
;;           (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
;;           (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
;;           (org        . ("https://github.com/milisims/tree-sitter-org"))
;;           (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
;;           (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
;;           (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
;;           (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
;;           (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
;;           (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
;;           (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
;;           (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
;;           (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
;;           (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
;;           (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))
;;   (add-to-list 'major-mode-remap-alist '(sh-mode         . bash-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(c-mode          . c-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(c++-mode        . c++-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(c-or-c++-mode   . c-or-c++-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(css-mode        . css-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(js-mode         . js-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(java-mode       . java-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(js-json-mode    . json-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(makefile-mode   . cmake-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(python-mode     . python-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(ruby-mode       . ruby-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(conf-toml-mode  . toml-ts-mode))
;;   (add-to-list 'auto-mode-alist '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode))
;;   (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;;   (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
;;   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;;   (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;;   (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))
;;   :config
;;   (setq treesit-auto-install 'prompt)
;;   (global-treesit-auto-mode)
;;   ;; 使得 org src 可以自动映射到 ts 模式
;;   (defun +remap-mode (mode)
;;     "make org-src-get-lang-mode respect major-mode-remap-alist"
;;     (treesit-auto--set-major-remap)
;;     (alist-get mode major-mode-remap-alist mode)
;;     )
;;   (advice-add 'org-src-get-lang-mode :filter-return #'+remap-mode)
;;   )

;; (use-package stickyfunc-enhance
;;   :straight t
;;   :init
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   (require 'stickyfunc-enhance)
;;   (setq semantic-stickyfunc-sticky-classes '(function type variable))
;;   :hook
;;   (prog-mode . semantic-mode))

;; chatgpt 支持
(use-package chatgpt-shell
  :straight t
  :config
  (setq chatgpt-shell-api-url-base "http://ipads.chat.gpt:3006")
    ;; 设置代理
    (setq chatgpt-shell-additional-curl-options '("-x" "http://127.0.0.1:7897"))

  ;; (setq chatgpt-shell-api-url-path "")
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  (setq chatgpt-shell-prompt-query-response-style #'shell)
  (setq chatgpt-shell-prompt-header-describe-code "What does the following code do? Use chinese to answer it")
  (setq chatgpt-shell-model-versions '("gpt-4o" "gpt-4o-mini"))
  (setq chatgpt-shell-model-version 1)
  (setq chatgpt-shell-root-path (expand-file-name "var/" user-emacs-directory))
  :bind
  ("C-c q" . chatgpt-shell-explain-code)
  ;; ("C-c q" . chatgpt-shell)
  ;; (global-set-key (kbd "C-d") 'chatgpt-shell)
  ("C-d" . chatgpt-shell)
  ;; M-n/p 可以查询历史
  (:map chatgpt-shell-mode-map
        ("M-<return>" . chatgpt-shell-newline)
        ("<return>" . chatgpt-shell-submit)
        ("C-l" . chatgpt-shell-clear-buffer)))
