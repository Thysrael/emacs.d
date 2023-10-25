;;; -*- lexical-binding: t -*-

;;; general
;; 让文本居中并限制宽度
(use-package visual-fill-column
  :hook ((markdown-mode markdown-view-mode org-mode) . +center-text)
  :config
  (defun +center-text ()
    (visual-fill-column-mode)
    (setq visual-fill-column-center-text t)))

;; 可以给没有加空格的文本加上空格
(use-package pangu-spacing)


;;; markdown
;; markdown-mode
(use-package markdown-mode
  :custom-face
  ;; 这里是对于 org-mode level 的定义，可能在切换主题时出现错误
  (markdown-code-face ((t (:inherit nil))))
  (markdown-header-face-1 ((t (:foreground "#c099ff" :weight bold :height 1.4 :family "LXGW WenKai"))))
  (markdown-header-face-2 ((t (:inherit outline-1 :extend nil :weight bold :height 1.3 :family "LXGW WenKai"))))
  (markdown-header-face-3 ((t (:inherit outline-2 :extend nil :weight bold :height 1.2 :family "LXGW WenKai"))))
  (markdown-header-face-4 ((t (:inherit outline-3 :extend nil :weight bold :height 1.1 :family "LXGW WenKai"))))
  (markdown-pre-face ((t (:inherit org-block :foreground "#c3e88d"))))
  (markdown-inline-code-face ((t (:inherit markdown-pre-face :extend nil))))
  :bind
  ("C-c C-v" . +toggle-markdown-mode)
  :config
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-header-scaling t
        markdown-asymmetric-header t
        markdown-nested-imenu-heading-index t
        markdown-fontify-code-blocks-natively t)
  (add-to-list 'markdown-code-lang-modes '("verilog" . verilog-mode))
  (add-to-list 'markdown-code-lang-modes '("c" . c-mode))
  (add-to-list 'markdown-code-lang-modes '("sh" . shell-script-mode))
  (add-to-list 'markdown-code-lang-modes '("shell" . shell-script-mode))
  (add-to-list 'markdown-code-lang-modes '("bash" . shell-script-mode))
  (defun +toggle-markdown-mode ()
  "Toggle between markdown-mode and markdown-view-mode."
  (interactive)
  (if (eq major-mode 'markdown-mode)
      (markdown-view-mode)
    (markdown-mode)))
  :hook
  (markdown-mode . (lambda () (setq line-spacing 0.25)))
  )

;;; org
;; 预览 LaTeX 公式
;; (use-package org-fragtog
;;   :hook ((org-mode . org-fragtog-mode)))

;; org-mode
(use-package org-mode
  :straight nil
  :custom-face
  (org-quote ((t (:inherit org-block-begin-line)))) ; 设置 qoute 的格式
  :hook
  (org-mode . (lambda () (setq line-spacing 0.25)))
  :init
  (setq org-startup-indented t) ; 设置缩进
  (setq org-fontify-quote-and-verse-blocks t) ; 高亮引用
  (setq org-fontify-quote-and-verse-blocks t) ; 高亮标题
  (setq org-src-tab-acts-natively t) ; 让源码块中的 tab 行为正常
  (setq org-src-preserve-indentation nil) ; 让缩进正常
  (setq org-ellipsis "…") ; 设置折叠提示符
  (setq org-use-sub-superscripts "{}") ; 用 {} 表示上下标
  (setq org-pretty-entities t) ; 设置一些连体字和上下标
  (setq org-hide-emphasis-markers t) ; 隐藏格式控制符
  (setq org-link-descriptive t) ; 显示链接的描述而非 URL
  (setq org-special-ctrl-a/e t) ; 在标题处 Ctrl-a 会忽略 *
  :hook
  (org-mode . org-num-mode) ; 添加标题序号
  )

(custom-set-faces
 ;; 这里是对于 org-mode level 的定义，可能在切换主题时出现错误
 '(org-document-title ((t (:foreground "#c099ff" :weight bold :height 1.3 :family "LXGW WenKai"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight bold :height 1.25 :family "LXGW WenKai"))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :weight bold :height 1.18 :family "LXGW WenKai"))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :weight bold :height 1.10 :family "LXGW WenKai")))))

;; ;; 现代化图标
;; (use-package org-modern
;;   :after org
;;   :hook ((org-mode . org-modern-mode)
;;          (org-agenda-finalize . org-modern-agenda-mode)))

;; 表格对齐
(use-package valign
  :custom
  (valign-fancy-bar t)
  :hook
  (org-mode . valign-mode))

;; org-appear 可以实时渲染格式
(use-package org-appear
  :straight nil
  :init
  (add-to-list 'load-path "~/.emacs.d/straight/repos/org-appear/")
  (require 'org-appear)
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t)
  )

;; 配置 mark 样式，主要是漂亮的 org-bold
(defface org-bold
  '((t :foreground "#d2268b"
       :weight bold
       :underline t
       :overline t))
  "Face for org-mode bold."
  :group 'org-faces )

(setq org-emphasis-alist
      '(("*" org-bold)
        ("/" italic)
        ("_" underline)
        ("=" ;; (:background "maroon" :foreground "white")
         org-verbatim verbatim)
        ("~" ;; (:background "deep sky blue" :foreground "MidnightBlue")
         org-code verbatim)
        ("+" (:strike-through t))))


;;; 让中文行内格式显示 https://emacs-china.org/t/org-mode/22313?u=vagrantjoker
;; (font-lock-add-keywords 'org-mode
;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
;;                         'append)
