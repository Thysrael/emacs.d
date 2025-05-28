;;; -*- lexical-binding: t -*-

;;; general
;; 让文本居中并限制宽度
(use-package visual-fill-column
  ;; :straight (:protocol https)
  :ensure t
  :hook ((markdown-mode markdown-view-mode org-mode eww-mode gfm-mode gfm-view-mode LaTeX-mode) . +center-text)
  :config
  (defun +center-text ()
    (interactive)
    (visual-fill-column-mode)
    (setq visual-fill-column-center-text t)))

;; 可以给没有加空格的文本加上空格
(use-package pangu-spacing
  :ensure t
  :hook
  (eww-mode . pangu-spacing-mode)
  (chatgpt-shell-mode . pangu-spacing-mode))

;;; markdown
;; markdown-mode
(use-package markdown-mode
  :ensure t
  :custom-face
  ;; 这里是对于 org-mode level 的定义，可能在切换主题时出现错误
  (markdown-code-face ((t (:inherit nil))))
  (markdown-header-face-1 ((t (:inherit org-level-1))))
  (markdown-header-face-2 ((t (:inherit org-level-2))))
  (markdown-header-face-3 ((t (:inherit org-level-3))))
  (markdown-header-face-4 ((t (:inherit org-level-4))))
  ;; (markdown-header-face-4 ((t (:inherit outline-3 :extend nil :weight bold :height 1.1 :family "LXGW WenKai"))))
  (markdown-pre-face ((t (:inherit org-code))))
  (markdown-inline-code-face ((t (:inherit markdown-pre-face :extend nil))))
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9)))) ; 标题前的 #
  (markdown-table-face ((t (:inherit org-table))))
  :bind
  (:map markdown-mode-map
        ("C-c C-v" . +toggle-markdown-mode)
        ("C-c C-b" . markdown-insert-bold)
        )
  :mode ("\\.md$" . gfm-mode)
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
  (add-to-list 'markdown-code-lang-modes '("c++" . c++-mode))
  (add-to-list 'markdown-code-lang-modes '("cpp" . c++-mode))
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
;; (use-package edit-indirect
;;   :straight t)

;;; org
;; org-mode
;; org-link-preview 是 org-toggle-inline-image 的代替
(use-package org
  :ensure t
  ;; :straight (:type built-in)
  ;; org latex preview https://abode.karthinks.com/org-latex-preview/
  ;; 原本的 org 的中英对齐功能有 bug
  ;; :straight `(org
  ;;             :fork (:host nil
  ;;                          :repo "https://git.tecosaur.net/tec/org-mode.git"
  ;;                          :branch "dev"
  ;;                          :remote "tecosaur")
  ;;             :files (:defaults "etc")
  ;;             :build t
  ;;             :pre-build
  ;;             (with-temp-file "org-version.el"
  ;;               (require 'lisp-mnt)
  ;;               (let ((version
  ;;                      (with-temp-buffer
  ;;                        (insert-file-contents "lisp/org.el")
  ;;                        (lm-header "version")))
  ;;                     (git-version
  ;;                      (string-trim
  ;;                       (with-temp-buffer
  ;;                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
  ;;                         (buffer-string)))))
  ;;                 (insert
  ;;                  (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
  ;;                  (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
  ;;                  "(provide 'org-version)\n")))
  ;;             :pin nil)
  :custom-face
  ;; (org-quote ((t (:inherit org-block-begin-line)))) ; 设置 qoute 的格式
  ;;                                       ; 这里是对于 org-mode level 的定义，可能在切换主题时出现错误
  ;; (org-document-title ((t (:foreground "#c099ff" :weight bold :height 1.3 :family "LXGW WenKai"))))
  (org-level-1 ((t (:inherit outline-1 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-level-2 ((t (:inherit outline-2 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-level-3 ((t (:inherit outline-3 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-level-4 ((t (:inherit outline-4 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-table ((t (:family "Sarasa Mono SC")))) ; 设置表格为中英等宽字体
  :custom
  ;; 图片粘贴功能
  (org-yank-image-save-method "./img")
  (org-yank-dnd-method 'file-link)
  (org-image-actual-width '(600))
  ;; (org-startup-with-inline-images t) ; 默认显示图片
  ;; (org-ellipsis "…") ; 设置折叠提示符
  (org-ellipsis "¶")
  (org-highlight-latex-and-related '(native))
  (org-startup-numerated t) ; 添加标题序号
  ;; 默认只展开标题行
  (org-startup-folded 'content)
  ;; 列表的下一级设置
  (org-list-demote-modify-bullet '(
								   ("-"  . "+")
                                   ("+"  . "1.")
								   ("1." . "1)")
								   ))
  (org-preview-latex-image-directory "/tmp/ltximg/")
  (org-latex-create-formula-image-program 'dvisvgm)
  :hook
  (org-mode . (lambda () (setq line-spacing 0.25)))
  (org-mode . (lambda () (electric-indent-local-mode 0))) ; 可以解决列表空一行莫名其妙的缩进问题
  (org-mode . (lambda () (setq-local electric-pair-pairs
                                `(,@electric-pair-pairs
                                  (?\{ . ?\})
                                  (?\( . ?\))
                                  (?\[ . ?\])
                                  (?\$ . ?\$))
                                )))
  :config ; 这里其实应该放到 custom 中，但是不能放到 config 中
  (setq org-startup-indented t) ; 设置缩进
  (setq org-fontify-quote-and-verse-blocks t) ; 高亮引用
  (setq org-fontify-quote-and-verse-blocks t) ; 高亮标题
  (setq org-use-sub-superscripts "{}") ; 用 {} 表示上下标
  (setq org-pretty-entities t) ; 设置一些连体字和上下标
  (setq org-hide-emphasis-markers t) ; 隐藏格式控制符
  (setq org-link-descriptive t) ; 显示链接的描述而非 URL
  (setq org-special-ctrl-a/e t) ; 在标题处 Ctrl-a 会忽略 *
  (setq org-special-ctrl-k t) ; 在标题处 Ctrl-k 会删除整个标题
  (setq org-element-use-cache nil)
  (setq org-element-cache-persistent nil)
  ;; 根据不同屏幕显示图片不同大小
  (let ((hostname (system-name)))
    (if (string-equal hostname "banana")
        (setq org-image-actual-width '(1200))
      (setq org-image-actual-width '(600))))

  ;;   (setq org-latex-create-formula-image-program 'dvisvgm
  ;;         org-startup-with-latex-preview nil)
  (with-eval-after-load 'org
    (plist-put org-format-latex-options :scale 0.6))
  (push '("jupyter-python" . python) org-src-lang-modes)
  (defface org-bold
    '((t :foreground "#d2268b"
         :family "Sarasa Mono SC"
         :weight bold
         ;; :underline t
         ;; :overline t
         ))
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
        ;; ("C-c C-w" . org-copy-subtree)
        ("C-c C-q" . org-cut-subtree)
        ("C-c C-b" . org-emphasize)
        ("C-," . nil))
  )

;; 中文行内样式避免空格
;; 我只是单纯觉得不够 native
;; (use-package org
;;   :config
;;   ;; HACK: inline highlight for CJK
;;   (setq org-emphasis-regexp-components '("-[:space:]('\"{[:nonascii:][:alpha:]"
;;                                          "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
;;                                          "[:space:]"
;;                                          "."
;;                                          1))
;;   (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
;;   (org-element-update-syntax)
;;   (org-element--set-regexps)
;;
;;   (defun +org-do-emphasis-faces (limit)
;;     "Run through the buffer and emphasize strings."
;;     (let ((quick-re (format "\\([%s]\\|^\\)\\([~=*/_+]\\).*?[~=*/_+]"
;;                             (car org-emphasis-regexp-components))))
;;       (catch :exit
;;         (while (re-search-forward quick-re limit t)
;;           (let* ((marker (match-string 2))
;;                  (verbatim? (member marker '("~" "="))))
;;             (when (save-excursion
;;                     (goto-char (match-beginning 0))
;;                     (and
;;                      ;; HACK: Do not match latex fragments.
;;                      (not (texmathp))
;;                      ;; Do not match table hlines.
;;                      (not (and (equal marker "+")
;;                                (org-match-line
;;                                 "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
;;                      ;; Do not match headline stars.  Do not consider
;;                      ;; stars of a headline as closing marker for bold
;;                      ;; markup either.
;;                      (not (and (equal marker "*")
;;                                (save-excursion
;;                                  (forward-char)
;;                                  (skip-chars-backward "*")
;;                                  (looking-at-p org-outline-regexp-bol))))
;;                      ;; Match full emphasis markup regexp.
;;                      (looking-at (if verbatim? org-verbatim-re org-emph-re))
;;                      ;; Do not span over paragraph boundaries.
;;                      (not (string-match-p org-element-paragraph-separate
;;                                           (match-string 2)))
;;                      ;; Do not span over cells in table rows.
;;                      (not (and (save-match-data (org-match-line "[ \t]*|"))
;;                                (string-match-p "|" (match-string 4))))))
;;               (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
;;                           (m (if org-hide-emphasis-markers 4 2)))
;;                 (font-lock-prepend-text-property
;;                  (match-beginning m) (match-end m) 'face face)
;;                 (when verbatim?
;;                   (org-remove-flyspell-overlays-in
;;                    (match-beginning 0) (match-end 0))
;;                   (remove-text-properties (match-beginning 2) (match-end 2)
;;                                           '(display t invisible t intangible t)))
;;                 (add-text-properties (match-beginning 2) (match-end 2)
;;                                      '(font-lock-multiline t org-emphasis t))
;;                 (when (and org-hide-emphasis-markers
;;                            (not (org-at-comment-p)))
;;                   (add-text-properties (match-end 4) (match-beginning 5)
;;                                        '(invisible t))
;;                   (add-text-properties (match-beginning 3) (match-end 3)
;;                                        '(invisible t)))
;;                 (throw :exit t))))))))
;;   (advice-add #'org-do-emphasis-faces :override #'+org-do-emphasis-faces)
;;   (defun +org-element--parse-generic-emphasis (mark type)
;;     "Parse emphasis object at point, if any.
;;
;; MARK is the delimiter string used.  TYPE is a symbol among
;; `bold', `code', `italic', `strike-through', `underline', and
;; `verbatim'.
;;
;; Assume point is at first MARK."
;;     (save-excursion
;;       (let ((origin (point)))
;;         (unless (bolp) (forward-char -1))
;;         (let ((opening-re
;;                (rx-to-string
;;                 `(seq (or line-start (any space ?- ?\( ?' ?\" ?\{ nonascii))
;;                       ,mark
;;                       (not space)))))
;;           (when (looking-at opening-re)
;;             (goto-char (1+ origin))
;;             (let ((closing-re
;;                    (rx-to-string
;;                     `(seq
;;                       (not space)
;;                       (group ,mark)
;;                       (or (any space ?- ?. ?, ?\; ?: ?! ?? ?' ?\" ?\) ?\} ?\\ ?\[
;;                                nonascii)
;;                           line-end)))))
;;               (when (re-search-forward closing-re nil t)
;;                 (let ((closing (match-end 1)))
;;                   (goto-char closing)
;;                   (let* ((post-blank (skip-chars-forward " \t"))
;;                          (contents-begin (1+ origin))
;;                          (contents-end (1- closing)))
;;                     (list type
;;                           (append
;;                            (list :begin origin
;;                                  :end (point)
;;                                  :post-blank post-blank)
;;                            (if (memq type '(code verbatim))
;;                                (list :value
;;                                      (and (memq type '(code verbatim))
;;                                           (buffer-substring
;;                                            contents-begin contents-end)))
;;                              (list :contents-begin contents-begin
;;                                    :contents-end contents-end)))))))))))))
;;   (advice-add #'org-element--parse-generic-emphasis :override #'+org-element--parse-generic-emphasis))

;; 禁用一些 org-modules 的加载
;; (with-eval-after-load 'org
;;   (setq org-modules (cl-set-difference org-modules '(ol-gnus ol-eww))))

;; 预览 LaTeX 公式
;; (use-package org-fragtog
;;   :straight t
;;   :hook
;;   ((org-mode . org-fragtog-mode))
;;   :custom
;;   (org-preview-latex-image-directory "/tmp/ltximg/"))

;; 预览 LaTeX
;; (use-package org-latex-preview
;;   :after org
;;   :config
;;   ;; Increase preview width
;;   (plist-put org-latex-preview-appearance-options
;;              :zoom 1.2)
;;   ;; Block C-n and C-p from opening up previews when using auto-mode
;;   (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
;;   (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)
;;   ;; Enable consistent equation numbering
;;   (setq org-latex-preview-numbered t)
;;   :hook
;;   (org-mode . org-latex-preview-auto-mode)
;;   )

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
;; (use-package valign
;;   :straight t
;;   :after (:any org markdown-mode)
;;   :custom
;;   (valign-fancy-bar t)
;;   :hook
;;   (org-mode . valign-mode)
;;   (markdown-mode . valign-mode))

;; org-appear 可以实时渲染格式
(use-package org-appear
  :ensure t
  :hook ((org-mode . org-appear-mode))
  :config
  (setq
   org-hide-emphasis-markers t

   org-appear-autosubmarkers t
   org-appear-autoentities t
   org-appear-autokeywords t
   org-appear-inside-latex t
   org-appear-autolinks t
   org-appear-delay 0.1

   org-appear-trigger 'manual)

  (add-hook! org-mode-hook :call-immediately
    (defun +org-add-appear-hook ()
      (add-hook 'meow-insert-enter-hook #'org-appear-manual-start nil t)
      (add-hook 'meow-insert-exit-hook #'org-appear-manual-stop nil t)))
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

;; 中文字数统计
;; Ns: 除了空白符以外的字符（字数）
;; AL: 并不排除空白符以外的字符（字数）
;; Ln: 行数
;; An: 意义不明，应该是单词数
;; Ha: 中文字符数（字数）
;; Wc: An + Ha
(use-package advance-words-count
  :vc (advance-words-count :url "https://github.com/Thysrael/advance-words-count.el" :rev "master")
  :commands advance-words-count
  )

;; org 智能列表
(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package gnuplot
  :ensure t)

(use-package org-src
  :init
  ;; 设置代码块的默认头参数
  (setq org-babel-default-header-args
        '(
          (:eval    . "never-export")     ; 导出时不执行代码块
          (:session . "none")
          (:results . "value verbatim output replace") ; 多次执行结果覆盖，结果是代码而不是表格
          (:exports . "both")             ; 导出代码和结果
          (:cache   . "no")
          (:noweb   . "no")
          (:hlines  . "no")
          (:tangle  . "no")               ; 不写入文件
          ))
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)
  ;;    (gnuplot .t)
  ;;    (python . t)
  ;;    (C               . t)
  ;;    ))
  :custom
  (org-src-tab-acts-natively t) ; 让源码块中的 tab 行为正常
  ;; 执行前是否需要确认
  (org-confirm-babel-evaluate nil)
  ;; 代码块默认前置多少空格
  (org-src-preserve-indentation nil) ; 让缩进正常
  (org-edit-src-content-indentation 0) ; 虽然丑了一些，但是方便粘贴
  (org-babel-load-languages '((python          . t)
                              (C               . t)
                              (emacs-lisp      . t)
                              (shell           . t)
                              (gnuplot         . t)
                              ))
  )
;; (use-package org-latex-impatient
;;   :after org
;;   :hook (org-mode . org-latex-impatient-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin
;;         ;; location of tex2svg executable
;;         (no-littering-expand-var-file-name "org-latex-impatient/node_modules/mathjax-node-cli/bin/tex2svg")))

;; (use-package hexo
;;   :straight t
;;   :config
;;   (setq hexo-root-dir "~/blog/")
;;   :bind
;;   ("C-c h" . hexo))

;; 更好的 LaTeX 支持
;; emacs 内置的是 latex-mode, auctex 提供的是 LaTeX-mode
;; C-c ret 是插入命令
;; C-c C-c 编译，选择 XeLaTex 命令，C-c C-v 预览当前位置，C-c C-r 局部编译，C-c ` 跳转到编译错误位置
;; C-c * 快速选择一章，C-c . 快速选择一个环境
(use-package latex
  :ensure auctex
  :hook ((TeX-mode . prettify-symbols-mode)
         (TeX-mode . (lambda ()
                       (push '("\\lnot" . ?¬) prettify-symbols-alist)
                       (prettify-symbols-mode 1))))
  :config
  
  (setq TeX-parse-self t             ; parse on load
        TeX-auto-save t              ; parse on save
        ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Just save, don't ask before each compilation.
        TeX-save-query nil
        )
  ;; 在完成编译后刷新 pdf 文件
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  ;; )
  (setq-default TeX-engine 'xetex)

  ;; (setq-default TeX-master nil) ;; 需要选择编译对象
  ;; shell-escape 用于允许 LaTeX 编译器在编译过程中执行一些系统命令
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' -shell-escape %t" TeX-run-TeX nil t))

  ;; 设置 eaf 为阅读器
  (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))
  ;; Use pdf-tools to open PDF files
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  ;; (add-to-list 'font-latex-match-reference-keywords-local '("upcite" "*[[{"))

  :custom-face
  ;; (font-latex-sedate-face ((t (:inherit org-bold))))
  (font-latex-sedate-face ((t (:foreground "#ff5555" :weight bold))))
  )

;; 快捷输入法，
;; 按 ` 可以输入数学符号
;; 按 ' 后再按 b 可以加粗，应该是样式控制符。
;; 输入一些字符后按 tab 也可以快速模板展开，类似于 snippet 的效果
;; 比如输入 env 后按 tab ，要避免 corfu 的补全。
;; C-c { 是插入环境，也可以 env 后按 tab
;; tab 也可以用于快速跳转
(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . cdlatex-mode))

;; 为 latex 提供折叠大纲功能
(use-package outline
  :hook
  (LaTeX-mode . outline-minor-mode)
  :bind
  (:map outline-minor-mode-map
        ("C-c C-o" . outline-cycle)
        ("C-c [" . nil)))

(use-package bibtex-mode
  :bind
  (:map bibtex-mode-map
        ("C-c C-f" . bibtex-reformat))
  )

;; 都存在对于高亮块不支持，解析不完善的情况，可能以后会好吧
;; (use-package markdown-ts-mode
;;   :straight t
;;   :mode ("\\.md\\'" . markdown-ts-mode))

;; (use-package md-ts-mode
;;   :straight (:type git :host github :repo "eki3z/md.el")
;;   :mode ("\\.md\\'" . md-ts-mode))

;; 似乎不会实时刷新数据库
;; (use-package ebib
;;   :straight t
;;   :config
;;   (add-to-list 'ebib-citation-commands
;;                '(LaTeX-mode
;;                  (("upcite" "\\upcite{%K}")
;;                   ("cite" "\\upcite{%K}")
;;                   )))
;;   :custom
;;   (ebib-preload-bib-files '("./bibs.bib"))
;;   :bind
;;   ("C-c [" . ebib-insert-citation)
;;   )

;; (use-package citar
;;   :straight t
;;   :custom
;;   (citar-bibliography '("./bibs.bib"))
;;   (citar-latex-default-cite-command "upcite")
;;   :hook
;;   (LaTeX-mode . citar-capf-setup)
;;   (org-mode . citar-capf-setup)
;;   :bind
;;   ("C-c [" . citar-insert-citation)
;;   :custom
;;   (citar-latex-cite-commands
;;    '((("upcite"))))
;;   )

;; (use-package consult-bibtex
;;   :straight (consult-bibtex :host github
;;                             :repo "mohkale/consult-bibtex")
;;   :config
;;   (setq bibtex-completion-bibliography
;;         '("./bibs.bib"))
;;   :bind
;;   ("C-c [" . consult-bibtex)
;;   )
