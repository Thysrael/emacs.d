;;; -*- lexical-binding: t -*-

;;; 代码补全
;; 可以使用 M-/ 进行简单的补全
(use-package dabbrev
  :straight t
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :bind
  ("M-'" . dabbrev-expand))

;;; corfu & tempel 方案，缺点是会自动触发 yasnippet
(use-package tempel
  :straight t
  :bind (:map tempel-map
              ("RET" . tempel-next)
              ("S-<RET>" . tempel-previous))
  :hook (((prog-mode text-mode) . +tempel-setup-capf)
         ((prog-mode text-mode) . tempel-abbrev-mode))
  :config
  (defun +tempel-setup-capf ()
    (push #'tempel-complete completion-at-point-functions))

  (setq tempel-trigger-prefix "`"
        tempel-path (no-littering-expand-etc-file-name "tempel-templates"))
  )


(use-package tempel-collection
  :straight t
  :after tempel)


;; ;; [corfu] compleletion frontend
;; (use-package corfu
;;   :straight (:files (:defaults "extensions/*.el"))
;;   :hook (((prog-mode conf-mode yaml-mode shell-mode eshell-mode) . corfu-mode)
;;          ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
;;          (minibuffer-setup . corfu-enable-in-minibuffer))
;;   :bind (:map corfu-map
;;               ([tab] . corfu-next)
;;               ([backtab] . corfu-previous))
;;   :config
;;   (setq corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
;;         corfu-auto t                 ;; Enable auto completion
;;         corfu-separator "&"          ;; Orderless field separator
;;         corfu-auto-prefix 2          ;; minimun prefix to enable completion
;;         corfu-preview-current nil
;;         corfu-on-exact-match nil 
;;         corfu-auto-delay 0.1)

;;   ;; Transfer completion to the minibuffer
;;   (defun corfu-move-to-minibuffer ()
;;     (interactive)
;;     (let ((completion-extra-properties corfu--extra)
;;           completion-cycle-threshold completion-cycling)
;;       (apply #'consult-completion-in-region completion-in-region--data)))

;;   ;; Completing in the minibuffer
;;   (defun corfu-enable-in-minibuffer ()
;;     "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;     (when (where-is-internal #'completion-at-point (list (current-local-map)))
;;       (corfu-mode 1)))
;;   )

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook (((tex-mode prog-mode conf-mode yaml-mode shell-mode eshell-mode org-mode markdown-mode) . corfu-mode)
         ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
         (minibuffer-setup . +corfu-enable-in-minibuffer)
         ;; 在 insert 模式退出时关闭 corfu 补全窗口
         (meow-insert-exit . (lambda () (corfu-quit))))

  :bind (:map corfu-map
              ("A-m" . +corfu-move-to-minibuffer)
              ("RET" . nil))
  :config
  (setq corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
        corfu-auto t                 ;; Enable auto completion
        corfu-auto-prefix 1          ;; minimun prefix to enable completion
        corfu-preview-current nil
        corfu-auto-delay 0.1)

  ;; Transfer completion to the minibuffer
  (defun +corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  ;; Completing in the minibuffer
  (defun +corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  )

(use-package corfu-history
  :straight nil
  :after corfu
  :init
  (corfu-history-mode 1)
  :config
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
  )


(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :init
  (corfu-popupinfo-mode 1)
  :config
  (setq corfu-popupinfo-delay '(1.0 . 1.0)))

;; 美化 corfu
(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; 这是与补全前端 corfu 配合的补全后端 cape
(use-package cape
  :straight t
  :hook ((corfu-mode . +corfu-add-cape-backends)
         ((tex-mode latex-mode markdown-mode) . +corfu-add-cape-tex-backends))
  :config
  (defun +corfu-add-cape-backends ()
    (add-to-list 'completion-at-point-functions #'cape-file :append)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev :append))

  (defun +corfu-add-cape-tex-backends ()
    (add-to-list 'completion-at-point-functions #'cape-tex :append))
  )

;;; company & yasnippet 方案
;; (use-package company
;;   :init
;;   (add-hook 'prog-mode-hook 'company-mode)
;;   :bind 
;;   (:map company-mode-map
;; 		 ([remap completion-at-point] . company-complete)
;; 		 :map company-active-map
;; 		 ([tab]     . company-complete-common-or-cycle)
;; 		 ([backtab] . company-select-previous-or-abort))
;;   :config
;;   (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-idle-delay 0)
;;   (setq company-selection-wrap-around t)
;;   (setq company-transformers '(company-sort-by-occurrence))
;;   )


;; (use-package yasnippet
;;   :after company
;;   :init 
;;   (yas-global-mode)
;;   :config
;;   ;; add company-yasnippet to company-backends
;;   (defun company-mode/backend-with-yas (backend)
;;     (if (and (listp backend) (member 'company-yasnippet backend))
;; 	backend
;;       (append (if (consp backend) backend (list backend))
;;               '(:with company-yasnippet))))
;;   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;   )

;; (use-package yasnippet-snippets)


;;; corfu & yasnippet 方案
;; ;; 提供模板
;; (use-package yasnippet-snippets
;;   :straight t)
;;
;; ;; 提供补全后端
;; (use-package yasnippet-capf
;;   :straight t)
;;
;; ;; snippet 功能
;; (use-package yasnippet
;;   :straight t
;;   :hook
;;   ((prog-mode text-mode) . yas-minor-mode-on)
;;   ((prog-mode text-mode) . +yas-setup-capf) ; 这里的顺序很关键，似乎 yas 必须在 corfu 之前才可以
;;   :config
;;   (defun +yas-setup-capf ()
;;     (setq-local completion-at-point-functions
;;                 (cons #'yasnippet-capf
;;                       completion-at-point-functions))) ; 在后端登记补全信息
;;   :custom
;;   (yas-keymap-disable-hook
;;    (lambda () (and (frame-live-p corfu--frame)
;;               (frame-visible-p corfu--frame)))) ; 使得 corfu 的优先级比 yas 高，使 tab 先满足 corfu
;;   :bind
;;   (:map yas-keymap
;;         ("<tab>" . nil)
;;         ("TAB" . nil)
;;         ("RET" . yas-next-field-or-maybe-expand)) ; 修改 <enter> 为下一个键位
;;   )
;;
;; ;; consult 模式的 yasnippet 检索
;; (use-package consult-yasnippet
;;   :straight t
;;   :bind
;;   ("C-c y" . consult-yasnippet))
;;
;; ;; 补全前端
;; (use-package corfu
;;   :straight
;;   (:files (:defaults "extensions/*.el")) ;; 可以减少加载 corfu 的默认配置
;;   :hook
;;   ((prog-mode text-mode conf-mode yaml-mode shell-mode eshell-mode) . corfu-mode)
;;   ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil))) ; 在 shell 模式取消补全
;;   :bind
;;   (:map corfu-map
;;         ;; ([tab] . corfu-next)
;;         ;; ([backtab] . corfu-previous)
;;         ("RET" . nil)
;;         )
;;   :init
;;   (setq corfu-cycle t                ; Enable cycling for `corfu-next/previous'
;;         corfu-auto t                 ; Enable auto completion
;;         ;; corfu-separator "&"          ; Orderless field separator
;;         corfu-auto-prefix 1          ; minimun prefix to enable completion
;;         corfu-preview-current t
;;         corfu-auto-delay 0
;;         corfu-on-exact-match nil     ; 解决 dd 自动展开的关键
;;         corfu-min-width 25
;;         corfu-preselect 'directory   ; 不知道为啥，这个可以避免自动选择，可以使 tab 更加方便
;;         )
;;   )
;;
;; ;; 使得 corfu 排序为历史频率排序
;; (use-package corfu-history
;;   :straight nil
;;   :after corfu
;;   :init
;;   (corfu-history-mode 1)
;;   :config
;;   (with-eval-after-load 'savehist
;;     (cl-pushnew 'corfu-history savehist-additional-variables))
;;   )
;;
;; ;; 弹出相关信息
;; (use-package corfu-popupinfo
;;   :straight nil
;;   :after corfu
;;   :init
;;   (corfu-popupinfo-mode 1)
;;   :config
;;   (setq corfu-popupinfo-delay '(1.0 . 1.0)))
;;
;; ;; 美化 corfu
;; (use-package nerd-icons-corfu
;;   :straight t
;;   :after corfu
;;   :init
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
;;
;; (use-package corfu-terminal
;;   :straight t
;;   :when (not (display-graphic-p))
;;   :after corfu
;;   :init (corfu-terminal-mode 1))
;;
;; ;; ;; TODO: 显示有问题
;; ;; (use-package corfu-popupinfo
;; ;;   :straight nil
;; ;;   :after corfu
;; ;;   :hook
;; ;;   (corfu-mode . corfu-popupinfo-mode)
;; ;;   :config
;; ;;   (setq corfu-popupinfo-delay '(1.0 1.0)))
;;
;; ;; 补全后端
;; (use-package cape
;;   :straight t
;;   :hook ((corfu-mode . +corfu-add-cape-backends)
;;          ((TeX-mode LaTeX-mode org-mode markdown-mode) . +corfu-add-cape-tex-backends))
;;   :config
;;   (defun +corfu-add-cape-backends ()
;;     (add-to-list 'completion-at-point-functions #'cape-file :append)
;;     (add-to-list 'completion-at-point-functions #'cape-dabbrev :append))
;;
;;   (defun +corfu-add-cape-tex-backends ()
;;     (add-to-list 'completion-at-point-functions #'cape-tex :append))
;;   )


;;; lsp-bridge & yasnippet 方案
;; (use-package yasnippet
;;   :hook
;;   ((prog-mode text-mode) . yas-minor-mode-on)
;;   )
;;
;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :config
;;   (setq acm-enable-citre t)
;;   (setq acm-enable-preview t)
;;   (setq lsp-bridge-enable-diagnostics nil)
;;   :hook
;;   ((prog-mode text-mode) . lsp-bridge-mode)
;;   )
