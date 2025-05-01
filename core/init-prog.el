;;; -*- lexical-binding: t -*-

(use-package prog-mode
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
  :ensure t
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
  :ensure t
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
;; (use-package citre
;;   :straight t
;;   :bind
;;   ;; (:map prog-mode-map
;;   ;;   ("C-t j" . +citre-jump)
;;   ;;   ("C-t k" . +citre-jump-back)
;;   ;;   ("C-t p" . citre-peek)
;;   ;;   ("C-t h" . citre-ace-peek)
;;   ;;   ("C-t u" . citre-update-this-tags-file)
;;   ;;   ("C-t n" . citre-create-tags-file))
;;   ("M-t" . transient-citre)
;;   :init
;;   (require 'citre-config)
;;   :config
;;   (setq citre-auto-enable-citre-mode-modes '(prog-mode)
;;         ;; citre-default-create-tags-file-location 'global-cache
;;         citre-use-project-root-when-creating-tags t
;;         citre-prompt-language-for-ctags-command t
;;         citre-enable-capf-integration t)
;;
;;   (defun +citre-jump ()
;;     "Jump to the definition of the symbol at point. Fallback to `xref-find-definitions'."
;;     (interactive)
;;     (condition-case _
;;         (citre-jump)
;;       (error (call-interactively #'xref-find-definitions))))
;;
;;   (defun +citre-jump-back ()
;;     "Go back to the position before last `citre-jump'. Fallback to `xref-go-back'."
;;     (interactive)
;;     (condition-case _
;;         (citre-jump-back)
;;       (error (call-interactively #'xref-go-back))))
;;
;;   ;; Use Citre xref backend as a [fallback]
;;   (defadvice! +citre--xref-fallback-a (fn &rest args)
;;     :around #'xref--create-fetcher
;;     (let ((fetcher (apply fn args))
;;           (citre-fetcher
;;            (let ((xref-backend-functions '(citre-xref-backend t)))
;;              (ignore xref-backend-functions)
;;              (apply fn args))))
;;       (lambda ()
;;         (or (with-demoted-errors "%s, fallback to citre"
;;               (funcall fetcher))
;;             (funcall citre-fetcher)))))
;;   (transient-define-prefix transient-citre ()
;;     [
;;      ("p" "Peek" citre-peek)
;;      ("h" "Ace Peek"citre-ace-peek)
;;      ("u" "Update Tags File" citre-update-this-tags-file)
;;      ("n" "Create Tags File" citre-create-tags-file)
;;      ]
;;     )
;;   )

(use-package eglot
  ;; :straight (eglot :type git
  ;;                 :host github
  ;;                 :repo "AkibAzmain/eglot"
  ;;                 :branch "semantic-tokens")
  :ensure t
  :custom
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-report-progress nil)
  ;; :custom-face
  ;; (eglot-highlight-symbol-face ((t (:inherit bold :family "JetBrainsMono Nerd Font"))))
  ;; :hook ((c-ts-mode c++-ts-mode) . eglot-ensure)
  :bind
  ("C-c l" . transient-hydra)
  :config
  (setq eglot-events-buffer-size 0
        eglot-connect-timeout 10
        eglot-autoshutdown t
        ;; use global completion styles
        completion-category-defaults nil)
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

;; 加速 eglot
;; 需要先安装 https://github.com/blahgeek/emacs-lsp-booster
;; 在使用 elgot 前先开启 eglot-booster
;; 不知道为什么必须手动开启
;; TODO: eglot-booster 无法放到 local 下，是因为 exec-path 的问题
;; 使用 tramp 的时候，要求远端也需要 emacs-lsp-booster, 类似于 rg 了
;; 否则就会导致 lsp server 无法启动
;; pyright 和 pylsp 都没有 inlay hint
(use-package eglot-booster
  :unless on-server
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev "main")
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
  :ensure t
  :config
  (setq eldoc-echo-area-display-truncation-message t
        eldoc-echo-area-prefer-doc-buffer t
        eldoc-echo-area-use-multiline-p nil
        eglot-extend-to-xref t)
  :bind
  ("M-;" . eldoc))

;; 格式化文档注释自动生成，和 treesit 不兼容
;; (use-package docstr
;;   :straight t
;;   ;; :straight
;;   ;; (docstr :type git :host github :repo "thysrael/docstr"
;;   ;;         :files (:defaults "langs/*.el"))
;;   :init
;;   (setq docstr-key-support t) ; 设置键入触发
;;   :config
;;   (docstr-faces-apply) ; 设置高亮
;;   ;; :hook
;;   ;; (after-init . global-docstr-mode)
;;   :custom
;;   (docstr-desc-summary "@summary ")
;;   (docstr-desc-param " ")
;;   (docstr-desc-return " ")
;;   )

;; 新的 ts 语法高亮支持
(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  ;; 使得 org src 可以自动映射到 ts 模式
  (defun +remap-mode (mode)
    "make org-src-get-lang-mode respect major-mode-remap-alist"
    (treesit-auto--set-major-remap)
    (alist-get mode major-mode-remap-alist mode)
    )
  (advice-add 'org-src-get-lang-mode :filter-return #'+remap-mode)
  )

;; (use-package stickyfunc-enhance
;;   :straight t
;;   :init
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   (require 'stickyfunc-enhance)
;;   (setq semantic-stickyfunc-sticky-classes '(function type variable))
;;   :hook
;;   (prog-mode . semantic-mode))
