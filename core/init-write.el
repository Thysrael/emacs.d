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
  :custom-face
  (org-quote ((t (:inherit org-block-begin-line)))) ; 设置 qoute 的格式
                                        ; 这里是对于 org-mode level 的定义，可能在切换主题时出现错误
  (org-document-title ((t (:foreground "#c099ff" :weight bold :height 1.3 :family "LXGW WenKai"))))
  (org-level-1 ((t (:inherit outline-1 :extend nil :weight bold :height 1.25 :family "LXGW WenKai"))))
  (org-level-2 ((t (:inherit outline-2 :extend nil :weight bold :height 1.18 :family "LXGW WenKai"))))
  (org-level-3 ((t (:inherit outline-3 :extend nil :weight bold :height 1.10 :family "LXGW WenKai"))))
  :custom
  (org-image-actual-width '(800))
  (org-startup-with-inline-images t) ; 默认显示图片
  (org-ellipsis " ⭍") ; 设置折叠提示符
  :hook
  (org-mode . (lambda () (setq line-spacing 0.25)))
  (org-mode . org-num-mode) ; 添加标题序号
  :init
  (setq org-startup-indented t) ; 设置缩进
  (setq org-fontify-quote-and-verse-blocks t) ; 高亮引用
  (setq org-fontify-quote-and-verse-blocks t) ; 高亮标题
  (setq org-src-tab-acts-natively t) ; 让源码块中的 tab 行为正常
  (setq org-src-preserve-indentation nil) ; 让缩进正常
  (setq org-use-sub-superscripts "{}") ; 用 {} 表示上下标
  (setq org-pretty-entities t) ; 设置一些连体字和上下标
  (setq org-hide-emphasis-markers t) ; 隐藏格式控制符
  (setq org-link-descriptive t) ; 显示链接的描述而非 URL
  (setq org-special-ctrl-a/e t) ; 在标题处 Ctrl-a 会忽略 *
  ;; 配置 org 行内样式
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
  :bind
  (:map org-mode-map
        ("C-c C-w" . org-copy-subtree)
        ("C-c C-q" . org-cut-subtree))
  )

;; 现代化图标
;; (use-package org-modern
;;   :after org
;;   :hook ((org-mode . org-modern-mode)
;;          (org-agenda-finalize . org-modern-agenda-mode)))

;; 表格对齐
(use-package valign
  :after org
  :custom
  (valign-fancy-bar t)
  :hook
  (org-mode . valign-mode)
  (markdown-mode . valign-mode))

;; org-appear 可以实时渲染格式
(use-package org-appear
  :straight nil
  :after org
  :init
  (add-to-list 'load-path "~/.emacs.d/straight/repos/org-appear/")
  ;; (require 'org-appear)
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t)
  )

;;; 让中文行内格式显示 https://emacs-china.org/t/org-mode/22313?u=vagrantjoker
;; (font-lock-add-keywords 'org-mode
;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
;;                         'append)

;; 超级漂亮 svg 图标
(use-package svg-tag-mode
  :after org
  :hook (org-mode . svg-tag-mode)
  :config
  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (setq svg-tag-action-at-point 'edit)

  (setq svg-lib-icon-collections
        `(("bootstrap" .
           "https://icons.getbootstrap.com/assets/icons/%s.svg")
          ("simple" .
           "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
          ("material" .
           "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
          ("octicons" .
           "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
          ("boxicons" .
           "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))

  (setq svg-tag-tags
        `(
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

          ;; Keywords
          ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
                                                 :face 'org-todo :margin 0 :radius 5))))
          ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
                                                 :face 'org-todo :margin 0 :radius 5))))
          ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
                                                 :face 'org-done :margin 0 :radius 5))))

          ("FIXME\\b" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))

          ;; beautify pagebreak in orgmode
          ("\\\\pagebreak" . ((lambda (tag) (svg-lib-icon "file-break" nil :collection "bootstrap"
                                                          :stroke 0 :scale 1 :padding 0))))

          )))
