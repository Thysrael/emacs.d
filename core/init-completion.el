;;; -*- lexical-binding: t -*-

;;; 代码补全
(use-package dabbrev
  :ensure t
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  )

;;; corfu & tempel 方案
(use-package tempel
  :ensure t
  :bind
  (:map tempel-map
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

(use-package corfu
  ;; :straight (:files (:defaults "extensions/*.el"))
  :ensure t
  :hook
  (((LaTeX-mode tex-mode prog-mode conf-mode yaml-mode shell-mode eshell-mode org-mode markdown-mode) . corfu-mode)
   ((eshell-mode shell-mode) . (lambda () (setq-local corfu-auto nil)))
   )
  :bind
  (:map corfu-map
        ("RET" . nil))
  :config
  (setq corfu-cycle t                ;; Enable cycling for `corfu-next/previous'
        corfu-auto t                 ;; Enable auto completion
        corfu-auto-prefix 1          ;; minimun prefix to enable completion
        corfu-on-exact-match nil
        corfu-preview-current nil
        corfu-auto-delay 0.1)
  )

(use-package corfu-history
  :after corfu
  :init
  (corfu-history-mode 1)
  :config
  (with-eval-after-load 'savehist
    (cl-pushnew 'corfu-history savehist-additional-variables))
  )

(use-package corfu-popupinfo
  :after corfu
  :init
  (corfu-popupinfo-mode 1)
  :config
  (setq corfu-popupinfo-delay '(1.0 . 1.0)))

(use-package corfu-terminal
  :if (display-graphic-p)
  :straight t
  :custom
  (corfu-terminal-mode 1))

;; 美化 corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; 这是与补全前端 corfu 配合的补全后端 cape
(use-package cape
  :ensure t
  :hook
  ((prog-mode . +corfu-add-cape-prog-backends)
   ((LaTeX-mode markdown-mode org-mode) . +corfu-add-cape-write-backends))
  :config
  ;; 编程用到的 cape
  (defun +corfu-add-cape-prog-backends ()
    (add-to-list 'completion-at-point-functions #'cape-file :append)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev :append)
    )
  ;; 写作用到的 cape
  (defun +corfu-add-cape-write-backends ()
    (add-to-list 'completion-at-point-functions #'cape-dict :append) ; 加入 dict 享受完美搜索
    (add-to-list 'completion-at-point-functions #'cape-file :append)
    ;; 数学符号补全，有了 auctex 后不需要
    ;; (add-to-list 'completion-at-point-functions #'cape-tex :append)
    )
  :custom
  (cape-dict-limit 20)
  )
