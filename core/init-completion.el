;;; -*- lexical-binding: t -*-

;;; 代码补全
;; 可以使用 M-/ 进行简单的补全
(use-package dabbrev
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; (use-package tempel
;;   :straight t
;;   :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
;;          ("M-*" . tempel-insert))
;;   :hook (((prog-mode text-mode) . +tempel-setup-capf)
;;          ((prog-mode text-mode) . tempel-abbrev-mode))
;;   :config
;;   (defun +tempel-setup-capf ()
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-complete
;;                       completion-at-point-functions)))

;;   (setq 
;;         tempel-path (expand-file-name "tempel-templates" user-emacs-directory))
;;   )


;; (use-package tempel-collection
;;   :straight t
;;   :after tempel)


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

;; (use-package corfu-history
;;   :straight nil
;;   :after corfu
;;   :init
;;   (corfu-history-mode 1)
;;   :config
;;   (with-eval-after-load 'savehist
;;     (cl-pushnew 'corfu-history savehist-additional-variables))
;;   )


;; (use-package corfu-popupinfo
;;   :straight nil
;;   :after corfu)


;; (use-package cape
;;   :straight t
;;   :hook ((corfu-mode . +corfu-add-cape-backends)
;;          ((TeX-mode LaTeX-mode org-mode markdown-mode) . +corfu-add-cape-tex-backends))
;;   :config
;;   (defun +corfu-add-cape-backends ()
;;     (add-to-list 'completion-at-point-functions #'cape-file :append)
;;     (add-to-list 'completion-at-point-functions #'cape-dabbrev :append))

;;   (defun +corfu-add-cape-tex-backends ()
;;     (add-to-list 'completion-at-point-functions #'cape-tex :append))
;;   )


(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :bind 
  (
		 :map company-mode-map
		 ([remap completion-at-point] . company-complete)
		 :map company-active-map
		 ([tab]     . company-complete-common-or-cycle)
		 ([backtab] . company-select-previous-or-abort))
  :config
  (setq company-minimum-prefix-length 1) ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))
  )


(use-package yasnippet
  :after company
  :hook
  ((prog-mode text-mode) . yas-minor-mode-on)
  :config
  ;; add company-yasnippet to company-backends
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package yasnippet-snippets)
