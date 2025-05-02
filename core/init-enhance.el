;;; -*- lexical-binding: t -*-

;;; minibuffer
;; minibuffer 垂直展示，或者严格意义上说，minibuffer 已经被 vertico 接管了
(use-package vertico
  ;; :ensure (vertico :files ("extensions/*.el")) ; 因为 vertico 本质是一堆包的集合，所以这里全部将他们加载
  :ensure t
  :bind
  (:map vertico-map
        ("TAB" . minibuffer-complete)
        ("<tab>" . minibuffer-complete) ; 必须有这两条才能使用 tab 补全
        ("C-r" . vertico-repeat-select) ; 可以查看有参数的命令历史并进行选择
        ("C-<return>" . vertico-exit-input)) ; 强行终止输入
  :hook
  ((after-init . vertico-mode))
  :defines
  (crm-separator)
  :config
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 15)

  ;; 这是在强化 minibuffer 的多行输入提示符，但是我还没有遇到过 minibuffer 的多行输入
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""  crm-separator)
                              (car args))
                      (cdr args))))
  (advice-add #'org-olpath-completing-read :around
              (lambda (&rest args)
                (minibuffer-with-setup-hook
                    (lambda () (setq-local completion-styles '(basic)))
                  (apply args))))
  )

;; 在 minibuffer 输入路径时，可以以目录为单位进行删除（具体的键位绑定我看不懂）
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

;; 记录有参数的命令历史
(use-package vertico-repeat
  :ensure nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

;; 增加补全图标
;; (use-package all-the-icons-completion
;;   :straight t
;;   :hook ((after-init . all-the-icons-completion-mode)
;;          (marginalia-mode . all-the-icons-completion-marginalia-setup))
;;   )

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook
  ((after-init . nerd-icons-completion-mode)
   (marginalia-mode . nerd-icons-completion-marginalia-setup))
  )

;; 在 minibuffer 中提供补全支持，有多重风格
(use-package orderless
  :ensure t
  :demand t
  :config
  ;; 根据辅助字符来选择不同的补全风格
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; 以 $ 结尾的 pattern 会指定结尾
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; 忽略单个 !，是为下一个匹配做准备
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; 以 ! 开头的 pattern 表示结果都是不匹配 pattern 的（反选）
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; makes the string match ignoring diacritics and similar inflections on characters，似乎是只有俄语那种有音标的语言会需要
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; 首字母匹配
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; 不启动正则匹配，只使用文本匹配
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; flex 风格：字符必须按照给定的顺序出现，但不一定要连续出现
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
     ;; Annotations 匹配注释，性能不佳，最好只用于过滤
     ((string-prefix-p "@" pattern) `(orderless-annotation . ,(substring pattern 1)))
     ((string-suffix-p "@" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
     ))

  ;; 远端文件补全只使用 basic 风格
  (defun +vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun +vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))

  (defun orderless+basic-all (str table pred point)
    (or (orderless-all-completions str table pred point)
        (completion-basic-all-completions str table pred point)))

  (defun orderless+basic-try (str table pred point)
    (or (completion-basic-try-completion str table pred point)
        (orderless-try-completion str table pred point)))

  (add-to-list 'completion-styles-alist
               '(orderless+basic
                 orderless+basic-try
                 orderless+basic-all
                 "Unholy mix of Orderless and Basic."))

  (setq completion-styles '(orderless+basic)
        completion-category-defaults nil
        completion-ignore-case t
        completion-category-overrides '((file (styles +vertico-basic-remote orderless+basic)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  )

;; 支持用中文拼音首字母缩写来搜索中文
(use-package pinyinlib
  :ensure t
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; 为 minibuffer 的选项提供更加详细的信息
(use-package marginalia
  :ensure t
  :hook (vertico-mode . marginalia-mode))

;; 为 minibuffer 中出现的条目（不至于 minibuffer 条目）
;; 可以用 C-h 查看所有的条目
;; 右键菜单，根据不同的对象提供不同的 map ，也就是不同的命令
;; embark 在 org-headline 上很好用
(use-package embark
  :ensure t
  :bind (("C-;" . embark-act)
         ("C-c ; e" . embark-export)
         ("C-c ; c" . embark-collect)
         ;; custom
         :map embark-general-map
         ("a" . consult-line)
         ("s" . consult-ripgrep)
         :map embark-variable-map
         ("a" . consult-line)
         ("s" . consult-ripgrep)
         :map embark-symbol-map
         ("a" . consult-line)
         ("s" . consult-ripgrep)
         :map embark-identifier-map
         ("a" . consult-line)
         ("s" . consult-ripgrep)
         :map embark-library-map
         ("a" . consult-line)
         ("s" . consult-ripgrep)
         :map embark-region-map
         ("a" . consult-line)
         ("s" . consult-ripgrep)

         :map minibuffer-local-map
         ("C-c C-e" . +embark-export-write)
         )
  :defines (wgrep-change-to-wgrep-mode)
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  :config
  (defun +embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
     Supports exporting consult-grep/consult-ripgrep to wgrep, file to wdeired,
     and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (let* ((edit-command
            (pcase-let ((`(,type . ,candidates)
                         (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('consult-ripgrep #'wgrep-change-to-wgrep-mode)
                ('file #'wdired-change-to-wdired-mode)
                ('consult-location #'occur-edit-mode)
                (x (user-error "embark category %S doesn't support writable export" x)))))
           (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
      (embark-export)))
  )

;; 为 embark 和 consutl 配合提供支持，可能就是提供了 C 选项
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

;; 使用 `C-u` 为前缀可以使得 consult-ripgrep 搜索 ignore 文件
(use-package consult
  :ensure t
  :bind
  ([remap bookmark-jump]                 . consult-bookmark)
  ([remap list-registers]                . consult-register)
  ([remap goto-line]                     . consult-goto-line)
  ([remap imenu]                         . consult-imenu)
  ("C-c i"                               . consult-imenu)
  ;; ("C-c I"                               . consult-imenu-multi)
  ;; ("C-c o"                               . consult-outline)
  ("C-c b"                               . consult-bookmark)
  ("C-c k"                               . consult-kmacro)
  ([remap locate]                        . consult-locate)
  ([remap load-theme]                    . consult-theme)
  ([remap man]                           . consult-man)
  ([remap recentf-open-files]            . consult-recent-file)
  ([remap switch-to-buffer]              . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
  ([remap yank-pop]                      . consult-yank-pop)
  ([remap project-switch-to-buffer]      . consult-project-buffer)
  ("C-c s"                               . consult-ripgrep) ; 全局搜索
  ("C-c p"                               . consult-fd) ; 全局文件查找
  ("C-s"                                 . consult-line) ; 局部搜索
  :config
  (setq consult-narrow-key "<"
        consult-async-min-input 2)

  ;; replace multi-occur with consult-multi-occur
  (advice-add #'multi-occur :override #'consult-multi-occur)

  ;; [consult-register] Configure the register formatting.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; [consult-xref] Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun +consult-ripgrep-single-file ()
    "Call `consult-ripgrep' for the current file (cann't just a buffer)."
    (interactive)
    (let ((consult-project-function (lambda (x) nil)))
      (consult-ripgrep (list (shell-quote-argument buffer-file-name)))))
  )

;; [consult-dir] Insert path quickly in minibuffer
(use-package consult-dir
  :ensure t
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
  )

;;; misc
;; 地址跳转设置
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; [ediff] Diff & patch
(use-package ediff
  :ensure nil
  :hook
  ((ediff-before-setup . +ediff-save-window-config)
   ((ediff-quit ediff-suspend) . +ediff-restore-window-config))
  :functions (outline-show-all)
  :config

  ;; unfold outlines when using ediff
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all))

  ;; Restore window config after quitting ediff
  (defvar +ediff-saved-window-config nil)
  (defun +ediff-save-window-config ()
    (setq +ediff-saved-window-config (current-window-configuration)))
  (defun +ediff-restore-window-config ()
    (when (window-configuration-p +ediff-saved-window-config)
      (set-window-configuration +ediff-saved-window-config)))

  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally
        ;; turn off whitespace checking
        ediff-diff-options "-w")
  )

;; ;; 增强帮助文档，启动有些卡顿
;; (use-package helpful
;;   :straight t
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-mode)
;;   :bind (([remap describe-command] . helpful-command)
;;          ("<f1> f" . helpful-callable)
;;          ("<f1> v" . helpful-variable)
;;          ("<f1> s" . helpful-symbol)
;;          ("<f1> S" . describe-syntax)
;;          ("<f1> m" . describe-mode)
;;          ("<f1> F" . describe-face)
;;          ([remap describe-key] . helpful-key))
;;   )

;; 使用 `ispell-buffer` 可以检查英文拼写
(use-package ispell
  :unless on-server
  :hook ((org-mode . org-skip-region-alist)
         (markdown-mode . markdown-skip-region-alist))
  :config
  ;; Don't spellcheck org blocks
  (defun org-skip-region-alist ()
    (make-local-variable 'ispell-skip-region-alist)
    (dolist (pair '((org-property-drawer-re)
                    ("~" "~") ("=" "=")
                    ("^#\\+BEGIN_SRC" "^#\\+END_SRC")
                    ("\\\\(" "\\\\)") ("\\[" "\\]")
                    ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))

  (defun markdown-skip-region-alist ()
    (make-local-variable 'ispell-skip-region-alist)
    (dolist (pair '(("`" "`")
                    ("^```" "^```")
                    ("{{" "}}")
                    ("\\\\(" "\\\\)") ("\\[" "\\]")
                    ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))

  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--run-together")
        ispell-dictionary "en_US")

  (setq ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir")
        ispell-personal-dictionary (expand-file-name "ispell/.pws" user-emacs-directory))
  )

;; buffer 管理
;; 可以用 consult-buffer 配合 embark 管理
;; (use-package ibuffer
;;   :straight t
;;   :bind (("C-x C-b" . ibuffer))
;;   :config
;;
;;   (setq ibuffer-show-empty-filter-groups nil
;;         ibuffer-use-other-window t))

;; [ibuffer-project] Group ibuffer's list by project root
;; (use-package ibuffer-project
;;   :straight t
;;   :hook (ibuffer . +ibuffer-project-activete)
;;   :config
;;   ;; HACK: Push temperary buffers in a standalone group
;;   (defun +ibuffer-project-activete ()
;;     "Activate ibuffer-project"
;;     (interactive)
;;     (let ((starred-name-filter '(starred-name . ""))
;;           (scratch-filter '(name . "^\\*scratch\\(.*\\)\\*$"))
;;           (ebib-filter '(or (mode . ebib-entry-mode)
;;                             (mode . ebib-index-mode)
;;                             (mode . ebib-log-mode)
;;                             (mode . ebib-multiline-mode)
;;                             (mode . ebib-strings-mode)))
;;           (elfeed-filter '(or (mode . elfeed-search-mode)
;;                               (mode . elfeed-show-mode)))
;;           (eww-filter '(or (mode . eww-mode)
;;                            (mode . eww-bookmark-mode)
;;                            (mode . eww-history-mode)
;;                            (mode . eww-buffers-mode)
;;                            (mode . eww-search-annotations-mode)))
;;           (eaf-filter '(or (mode . EAF/browser-mode)
;;                            (name . "^\\*eaf")
;;                            (name . "^\\EAF")))
;;           (chatgpt-filter '(mode . chatgpt-shell-mode))
;;           (eshell-pop-filter '(name . "^\\*Eshell-pop\\*$"))
;;           (telega-filter '(or (mode . telega-chat-mode)
;;                               (mode . telega-root-mode)
;;                               (mode . telega-image-mode)
;;                               (mode . telega-webpage-mode)))
;;           (xwidget-filter '(mode . xwidget-webkit-mode)))
;;       (setq ibuffer-filter-groups
;;             (mapcar (lambda (p) (cons (car p) `((and ,(car (cdr p)) (not ,starred-name-filter)))))
;;                     (ibuffer-project-generate-filter-groups)))
;;       ;; ChatGPT buffer should be added first to avoid being grouped into projects
;;       (add-to-list 'ibuffer-filter-groups (list "Telega" telega-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "Scratch" scratch-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "ChatGPT" chatgpt-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "Eww" eww-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "EAF" eaf-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "Xwidget" xwidget-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "Ebib" ebib-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "Elfeed" elfeed-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "Eshell-pop" eshell-pop-filter))
;;       (add-to-list 'ibuffer-filter-groups (list "Temporary buffers" starred-name-filter) :append)
;;       )
;;
;;     (unless (eq ibuffer-sorting-mode 'project-file-relative)
;;       (ibuffer-do-sort-by-project-file-relative))
;;     )
;;
;;   (define-ibuffer-column +size-h
;;     (:name "Size" :inline t)
;;     (cond
;;      ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
;;      ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
;;      ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
;;      (t (format "%8d" (buffer-size)))))
;;
;;
;;   (setq ibuffer-formats
;;         '((mark modified read-only locked " "
;;                 (name 25 25 :left :elide)
;;                 " "
;;                 (+size-h 8 -1 :right)
;;                 " "
;;                 (mode 16 16 :left :elide)
;;                 " "
;;                 project-file-relative)))
;;   )

(use-package picture-mode
  :bind
  (:map picture-mode-map
        ("C-f" . 'right-char)))

(use-package artist-mode
  :bind
  (:map artist-mode-map
        ("C-f" . 'right-char)))

;; burly-bookmark can save window configure
;; (use-package burly
;;   :hook
;;   (tab-bar-mode . burly-tabs-mode))

;; 使 f2 为宏计数器
(global-set-key (kbd "<f2>") 'kmacro-set-counter)

;; 映射中文输入
(cl-loop for prefix in '("C-" "M-" "s-" "H-")
         do
         (cl-loop for cpunc in '("，" "。" "？" "！" "；" "：" "、" "（" "）" "【" "】" "《" "》" "—")
                  for epunc in '("," "." "?" "!" ";" ":" "," "(" ")" "[" "]" "<" ">" "_")
                  do (define-key key-translation-map (kbd (concat prefix cpunc)) (kbd (concat prefix epunc)))))

;; copy from remote emacs with osc52
;; use system clipboard to replace the king ring of remote emacs
(use-package clipetty
  :if on-server
  :ensure t
  :hook (after-init . global-clipetty-mode)
  )

;; 支持终端 emacs 和外部程序的粘贴
(setq x-select-enable-clipboard t)

;; use xsel to copy/paste in emacs-nox
(unless window-system
  (when (getenv "DISPLAY")
    (defun xclip-cut-function (text &optional push)
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-selection" "clipboard" "-i")))
    (defun xclip-paste-function ()
      (let ((xclip-output (shell-command-to-string "xclip -selection clipboard -o")))
        (unless (string= (car kill-ring) xclip-output)
          xclip-output)))
    (setq interprogram-cut-function 'xclip-cut-function)
    (setq interprogram-paste-function 'xclip-paste-function)))

;; remove `Indentation setup for shell type zsh`
;; https://emacs.stackexchange.com/questions/52846/how-to-remove-message-indentation-setup-for-shell-type-sh
(advice-add 'sh-set-shell :around
            (lambda (orig-fun &rest args)
              (let ((inhibit-message t))
                (apply orig-fun args))))

;; shell 以交互方式工作（即会读取 `.bashrc`）
;; 不知道为什么 zsh 不可以
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")
