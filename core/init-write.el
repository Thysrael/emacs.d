;;; -*- lexical-binding: t -*-

;;; general
;; 让文本居中并限制宽度
(use-package visual-fill-column
  :straight t
  :hook ((markdown-mode markdown-view-mode org-mode eww-mode gfm-mode gfm-view-mode) . +center-text)
  :config
  (defun +center-text ()
    (interactive)
    (visual-fill-column-mode)
    (setq visual-fill-column-center-text t)))

;; 可以给没有加空格的文本加上空格
(use-package pangu-spacing
  :straight t
  :hook
  (eww-mode . pangu-spacing-mode))

;;; markdown
;; markdown-mode
(use-package markdown-mode
  :straight t
  :custom-face
  ;; 这里是对于 org-mode level 的定义，可能在切换主题时出现错误
  (markdown-code-face ((t (:inherit nil))))
  ;; (markdown-header-face-1 ((t (:foreground "#c099ff" :weight bold :height 1.4 :family "LXGW WenKai"))))
  ;; (markdown-header-face-2 ((t (:inherit outline-1 :extend nil :weight bold :height 1.3 :family "LXGW WenKai"))))
  ;; (markdown-header-face-3 ((t (:inherit outline-2 :extend nil :weight bold :height 1.2 :family "LXGW WenKai"))))
  ;; (markdown-header-face-4 ((t (:inherit outline-3 :extend nil :weight bold :height 1.1 :family "LXGW WenKai"))))
  (markdown-pre-face ((t (:inherit org-block :foreground "#c3e88d"))))
  (markdown-inline-code-face ((t (:inherit markdown-pre-face :extend nil))))
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9)))) ; 标题前的 #
  :bind
  (:map markdown-mode-map
        ("C-c C-v" . +toggle-markdown-mode)
        ("C-c C-b" . markdown-insert-bold)
        )
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-header-scaling t
        markdown-asymmetric-header t
        markdown-nested-imenu-heading-index t
        markdown-fontify-code-blocks-natively t)
  (add-to-list 'markdown-code-lang-modes '("verilog" . verilog-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("c" . c-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("c++" . c++-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("cpp" . c++-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("sh" . shell-script-mode))
  (add-to-list 'markdown-code-lang-modes '("shell" . shell-script-mode))
  (add-to-list 'markdown-code-lang-modes '("bash" . shell-script-mode))

  (defun +toggle-markdown-mode ()
    "Toggle between markdown-mode and markdown-view-mode."
    (interactive)
    (if (eq major-mode 'gfm-mode)
        (gfm-view-mode)
      (gfm-mode)))
    ;; markdown 模式实时渲染，最终没有用，略卡
  ;; (defvar nb/current-line '(0 . 0)
  ;;   "(start . end) of current line in current buffer")
  ;; (make-variable-buffer-local 'nb/current-line)
  ;;
  ;; (defun nb/unhide-current-line (limit)
  ;;   "Font-lock function"
  ;;   (let ((start (max (point) (car nb/current-line)))
  ;;         (end (min limit (cdr nb/current-line))))
  ;;     (when (< start end)
  ;;       (remove-text-properties start end
  ;;                               '(invisible t display "" composition ""))
  ;;       (goto-char limit)
  ;;       t)))
  ;;
  ;; (defun nb/refontify-on-linemove ()
  ;;   "Post-command-hook"
  ;;   (let* ((start (line-beginning-position))
  ;;          (end (line-beginning-position 2))
  ;;          (needs-update (not (equal start (car nb/current-line)))))
  ;;     (setq nb/current-line (cons start end))
  ;;     (when needs-update
  ;;       (font-lock-fontify-block 3))))
  ;;
  ;; (defun nb/markdown-unhighlight ()
  ;;   "Enable markdown concealling"
  ;;   (interactive)
  ;;   (markdown-toggle-markup-hiding 'toggle)
  ;;   (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
  ;;   (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
  :hook
  (gfm-mode . (lambda () (setq line-spacing 0.25)))
  ;; (gfm-mode . nb/markdown-unhighlight)
  )

;; markdown edit inderct
(use-package edit-indirect
  :straight t)

;;; org
;; org-mode
(use-package org
  :straight t
  ;; :straight (:type built-in)
  ;; :custom-face
  ;; (org-quote ((t (:inherit org-block-begin-line)))) ; 设置 qoute 的格式
  ;;                                       ; 这里是对于 org-mode level 的定义，可能在切换主题时出现错误
  ;; (org-document-title ((t (:foreground "#c099ff" :weight bold :height 1.3 :family "LXGW WenKai"))))
  ;; (org-level-1 ((t (:inherit outline-1 :extend nil :weight bold :height 1.25 :family "LXGW WenKai"))))
  ;; (org-level-2 ((t (:inherit outline-2 :extend nil :weight bold :height 1.18 :family "LXGW WenKai"))))
  ;; (org-level-3 ((t (:inherit outline-3 :extend nil :weight bold :height 1.10 :family "LXGW WenKai"))))
  :custom
  (org-image-actual-width '(800))
  (org-startup-with-inline-images t) ; 默认显示图片
  (org-ellipsis "…") ; 设置折叠提示符
  :hook
  (org-mode . (lambda () (setq line-spacing 0.25)))
  (org-mode . org-num-mode) ; 添加标题序号
  (org-mode . (lambda () (electric-indent-local-mode 0))) ; 可以解决列表空一行莫名其妙的缩进问题
  :config ; 这里其实应该放到 custom 中，但是不能放到 config 中
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
  (setq org-special-ctrl-k t) ; 在标题处 Ctrl-k 会删除整个标题
  (setq org-element-use-cache nil)
  (setq org-element-cache-persistent nil)
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
  (setq org-latex-create-formula-image-program 'dvisvgm
        org-startup-with-latex-preview nil)
  (plist-put org-format-latex-options :scale 1.0)
  :bind
  (:map org-mode-map
        ("C-c C-w" . org-copy-subtree)
        ("C-c C-q" . org-cut-subtree))
  )

;; 禁用一些 org-modules 的加载
;; (with-eval-after-load 'org
;;   (setq org-modules (cl-set-difference org-modules '(ol-gnus ol-eww))))

;; 预览 LaTeX 公式
(use-package org-fragtog
  :straight t
  :hook
  ((org-mode . org-fragtog-mode))
  :custom
  (org-preview-latex-image-directory "/tmp/ltximg/"))

;; 给 markdown 提供 latex 预览能力，不过很容易卡死
;; (use-package texfrag
;;   :hook
;;   ;; (gfm-view-mode . texfrag-mode)
;;   ;; (texfrag-mode . texfrag-document)
;;   :custom
;;   (texfrag-subdir "/tmp/texfrag"))

;; 现代化图标
;; (use-package org-modern
;;   :after org
;;   :hook ((org-mode . org-modern-mode)
;;          (org-agenda-finalize . org-modern-agenda-mode)))

;; 表格对齐
(use-package valign
  :after (:any org markdown-mode)
  :custom
  (valign-fancy-bar t)
  :hook
  (org-mode . valign-mode)
  (markdown-mode . valign-mode))

;; org-appear 可以实时渲染格式
(use-package org-appear
  :straight t
  :after org
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autolinks t
        org-appear-autokeywords t)
  )

;;; 让中文行内格式显示 https://emacs-china.org/t/org-mode/22313?u=vagrantjoker
;; (font-lock-add-keywords 'org-mode
;;                         '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
;;                            (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
;;                           ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
;;                            (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
;;                         'append)

;; 超级漂亮 svg 图标
;; (use-package svg-tag-mode
;;   :after org
;;   :hook (org-mode . svg-tag-mode)
;;   :config
;;   (defun svg-progress-percent (value)
;;     (svg-image (svg-lib-concat
;;                 (svg-lib-progress-bar (/ (string-to-number value) 100.0)
;;                                       nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                 (svg-lib-tag (concat value "%")
;;                              nil :stroke 0 :margin 0)) :ascent 'center))
;;
;;   (defun svg-progress-count (value)
;;     (let* ((seq (mapcar #'string-to-number (split-string value "/")))
;;            (count (float (car seq)))
;;            (total (float (cadr seq))))
;;       (svg-image (svg-lib-concat
;;                   (svg-lib-progress-bar (/ count total) nil
;;                                         :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
;;                   (svg-lib-tag value nil
;;                                :stroke 0 :margin 0)) :ascent 'center)))
;;
;;   (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
;;   (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
;;   (defconst day-re "[A-Za-z]\\{3\\}")
;;   (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
;;
;;   (setq svg-tag-action-at-point 'edit)
;;
;;   (setq svg-lib-icon-collections
;;         `(("bootstrap" .
;;            "https://icons.getbootstrap.com/assets/icons/%s.svg")
;;           ("simple" .
;;            "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
;;           ("material" .
;;            "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
;;           ("octicons" .
;;            "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
;;           ("boxicons" .
;;            "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))
;;
;;   (setq svg-tag-tags
;;         `(
;;           ;; Task priority
;;           ("\\[#[A-Z]\\]" . ( (lambda (tag)
;;                                 (svg-tag-make tag :face 'org-priority
;;                                               :beg 2 :end -1 :margin 0))))
;;
;;           ;; Progress
;;           ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
;;                                               (svg-progress-percent (substring tag 1 -2)))))
;;           ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
;;                                             (svg-progress-count (substring tag 1 -1)))))
;;
;;           ;; ;; Active date (with or without day name, with or without time)
;;           ;; (,(format "\\(<%s>\\)" date-re) .
;;           ;;  ((lambda (tag)
;;           ;;     (svg-tag-make tag :beg 1 :end -1 :margin 0))))
;;           ;; (,(format "\\(<%s \\)%s>" date-re day-time-re) .
;;           ;;  ((lambda (tag)
;;           ;;     (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
;;           ;; (,(format "<%s \\(%s>\\)" date-re day-time-re) .
;;           ;;  ((lambda (tag)
;;           ;;     (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
;;           ;;
;;           ;; ;; Inactive date  (with or without day name, with or without time)
;;           ;; (,(format "\\(\\[%s\\]\\)" date-re) .
;;           ;;  ((lambda (tag)
;;           ;;     (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
;;           ;; (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
;;           ;;  ((lambda (tag)
;;           ;;     (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
;;           ;; (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
;;           ;;  ((lambda (tag)
;;           ;;     (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
;;
;;           ;; Keywords
;;           ("TODO" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
;;                                                  :face 'org-todo :margin 0 :radius 5))))
;;           ("WORK" . ((lambda (tag) (svg-tag-make tag :height 0.8
;;                                                  :face 'org-todo :margin 0 :radius 5))))
;;           ("DONE" . ((lambda (tag) (svg-tag-make tag :height 0.8 :inverse t
;;                                                  :face 'org-done :margin 0 :radius 5))))
;;
;;           ("ABORT" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :margin 0 :crop-right t))))
;;
;;           ;; beautify pagebreak in orgmode
;;           ("\\\\pagebreak" . ((lambda (tag) (svg-lib-icon "file-break" nil :collection "bootstrap"
;;                                                           :stroke 0 :scale 1 :padding 0))))
;;
;;           )))

;; 方便得插入图片
(use-package org-download
  :straight t
  ;; :after (:any org markdown-mode)
  :after org
  :bind
  (:map org-mode-map
        ("C-c M" . hydra-org-download/body)
        ("C-c m" . org-download-clipboard))
  ;; (:map markdown-mode-map
  ;;       ("C-c m" . hydra-md-download/body))
  :config
  (advice-add 'org-download--dir-1 :override
              (lambda ()
                (or org-download-image-dir
                    (file-name-sans-extension
                     (file-name-nondirectory buffer-file-name))))) ; 将默认行为修改为修改文件夹

  (defun +org-download--fullname (link &optional ext)
    (let* ((default-filename (when (boundp 'org-download-file-format-function)
                               (funcall org-download-file-format-function
                                        (file-name-nondirectory
                                         (car (url-path-and-query
                                               (url-generic-parse-url link)))))))
           (filename (read-string (format "Enter file name [%s] : " default-filename)
                                  nil nil default-filename)) ; 修改此处可以使得插入图片前询问名字
           (dir (org-download--dir)))
      (when (string-match ".*?\\.\\(?:png\\|jpg\\)\\(.*\\)$" filename)
        (setq filename (replace-match "" nil nil filename 1)))
      (when ext
        (setq filename (concat filename "." ext)))
      (abbreviate-file-name
       (expand-file-name filename dir))))
  (advice-add 'org-download--fullname :override '+org-download--fullname)
  ;; 失败的 markdown 尝试，但是应该是可以完成的
  ;; (setq org-download-annotate-function (lambda (link) ""))
  ;; (advice-add 'org-download-org-mode-p (lambda () (or (eq major-mode 'org-mode) (when (derived-mode-p 'org-mode) t) (eq major-mode 'markdown-mode))))
  (defhydra hydra-org-download (:color blue)
    "Org-download: "
    ("c" org-download-clipboard "Clipboard")
    ("y" org-download-yank "Yank")
    ("r" org-download-rename-at-point "Rename")
    ("s" org-download-screenshot "Screenshot")
    ("d" org-download-delete "Delete")
    ("l" org-download-image "Download")
    ("e" org-download-edit "Edit"))

  ;; (defhydra hydra-md-download (:color blue)
  ;;   "Md-download: "
  ;;   ("c" org-download-clipboard "Clipboard")
  ;;   ("y" org-download-yank "Yank")
  ;;   ("s" org-download-screenshot "Screenshot"))

  :custom
  (org-download-screenshot-method "flameshot gui --raw > %s")
  ;; (org-download-image-dir "./img") ; 将存在指定文件夹下
  (org-download-image-dir nil) ; 用前面的 advice 修改后的行为变成了存在 org 文件同名文件夹中
  (org-download-heading-lvl nil))

;; 字数统计
;; Ns: 除了空白符以外的字符（字数）
;; AL: 并不排除空白符以外的字符（字数）
;; Ln: 行数
;; An: 意义不明，应该是单词数
;; Ha: 中文字符数（字数）
;; Wc: An + Ha
(use-package advance-words-count
  :after (:any org markdown-mode)
  :straight (:type git :host github :repo "LdBeth/advance-words-count.el"))

(use-package org-autolist
  :straight t
  :after org
  :hook (org-mode . org-autolist-mode))

;; (use-package org-latex-impatient
;;   :after org
;;   :hook (org-mode . org-latex-impatient-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin
;;         ;; location of tex2svg executable
;;         (no-littering-expand-var-file-name "org-latex-impatient/node_modules/mathjax-node-cli/bin/tex2svg")))

(use-package hexo
  :straight t
  :config
  (setq hexo-root-dir "~/blog/")
  :bind
  ("C-c h" . hexo))
