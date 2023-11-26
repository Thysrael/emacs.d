;;; -*- lexical-binding: t -*-

;;; minibuffer
;; minibuffer 垂直展示，或者严格意义上说，minibuffer 已经被 vertico 接管了
(use-package vertico
  :straight
  (:files (:defaults "extensions/*.el")) ; 因为 vertico 本质是一堆包的集合，所以这里全部将他们加载
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

                                        ; 这是在强化 minibuffer 的多行输入提示符，但是我还没有遇到过 minibuffer 的多行输入
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""  crm-separator)
                              (car args))
                      (cdr args))))

                                        ; 这里说的是 org-refile 只支持基本的补全模式，并不支持 orderless 这样的补全，这个修改可以完善它
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t)
  (advice-add #'org-olpath-completing-read :around
              (lambda (&rest args)
                (minibuffer-with-setup-hook
                    (lambda () (setq-local completion-styles '(basic)))
                  (apply args))))
  )

;; 在 minibuffer 输入路径时，可以以目录为单位进行删除（具体的键位绑定我看不懂）
(use-package vertico-directory
  :straight nil
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
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

;; 在 minibuffer 中提供补全支持，有多重风格
(use-package orderless
  :init (require 'orderless)
  :config
                                        ; 根据辅助字符来选择不同的补全风格
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
                                        ; 以 $ 结尾的 pattern 会指定结尾
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
                                        ; 忽略单个 !，是为下一个匹配做准备
     ((string= "!" pattern) `(orderless-literal . ""))
                                        ; 以 ! 开头的 pattern 表示结果都是不匹配 pattern 的（反选）
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
                                        ; makes the string match ignoring diacritics and similar inflections on characters，似乎是只有俄语那种有音标的语言会需要
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
                                        ; 首字母匹配
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
                                        ; 不启动正则匹配，只使用文本匹配
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
                                        ; flex 风格：字符必须按照给定的顺序出现，但不一定要连续出现
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

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
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; 为 minibuffer 的选项提供更加详细的信息
(use-package marginalia
  :hook (vertico-mode . marginalia-mode))

;; 为 minibuffer 中出现的条目（不至于 minibuffer 条目）
(use-package embark
  :straight t
  :bind (("C-;" . embark-act)
         ("C-c ; e" . embark-export)
         ("C-c ; c" . embark-collect)
         :map minibuffer-local-map
         ("C-c C-e" . +embark-export-write)
         :map embark-file-map
         ("s" . +reopen-file-with-sudo)
         ("g" . +embark-magit-status))
  :defines (wgrep-change-to-wgrep-mode)
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  :config
  (defun +embark-magit-status (file)
    "Run `magit-status` on repo containing the embark target."
    (interactive "GFile: ")
    (magit-status (locate-dominating-file file ".git")))

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

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package consult
  :bind
  ([remap bookmark-jump]                 . consult-bookmark)
  ([remap list-registers]                . consult-register)
  ([remap goto-line]                     . consult-goto-line)
  ([remap imenu]                         . consult-imenu)
  ("C-c i"                               . consult-imenu)
  ("C-c I"                               . consult-imenu-multi)
  ("C-c o"                               . consult-outline)
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
  )

;; [consult-dir] Insert path quickly in minibuffer
(use-package consult-dir
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
  :straight nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; [ediff] Diff & patch
(use-package ediff
  :straight nil
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

;; [ispell] spell checker
(use-package ispell
  :straight nil
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

;; burly-bookmark can save window configure
;; (use-package burly
;;   :hook
;;   (tab-bar-mode . burly-tabs-mode))

;; Bookmark Register Rectangle
(defhydra Cx-r (
                :hint nil ; 只显示注释字符串，不显示绑定信息
                :color blue ; 执行完一次后就退出
                :foreign-keys run ; 如果不在 hydra 按键内，则执行，并不退出 hydra
                )
  "
        Bookmark^^        Register^^        Rectangle^^
  --------------------------------------------------------
        [_l_] List        [_v_] List        [_M_] Mark
        [_m_] Mark        [_SPC_] Point     [_N_] Number
        [_b_] Jump        [_s_] Text        [_t_] String
        ^ ^               [_r_] Rectangle   [_o_] Space
        ^ ^               [_W_] Window      [_c_] Clear
        ^ ^               [_K_] Kmacro      [_k_] Kill
        [_q_] Quit        ^ ^               [_y_] Yank
  "
  ("m" bookmark-set-no-overwrite)
  ("b" bookmark-jump)
  ("l" bookmark-bmenu-list)
  ;; ("w" burly-bookmark-windows)

  ("v" consult-register)
  ("SPC" point-to-register)
  ("s" copy-to-register)
  ("r" copy-rectangle-to-register)
  ("W" window-configuration-to-register)
  ("K" kmacro-to-register)

  ("M" rectangle-mark-mode :color red) ; red 执行完后不退出
  ("N" rectangle-number-lines :color red)
  ("t" string-rectangle :color red)
  ("o" open-rectangle :color red)
  ("c" clear-rectangle :color red)
  ("k" kill-rectangle :color red)
  ("y" yank-rectangle :color red)

  ("q" nil))
(global-set-key (kbd "C-x r") 'Cx-r/body)

;; 使 f2 为宏计数器
(global-set-key (kbd "<f2>") 'kmacro-set-counter)

;; 映射中文输入
(cl-loop for prefix in '("C-" "M-" "s-" "H-")
         do
         (cl-loop for cpunc in '("，" "。" "？" "！" "；" "：" "、" "（" "）" "【" "】" "《" "》" "—")
                  for epunc in '("," "." "?" "!" ";" ":" "," "(" ")" "[" "]" "<" ">" "_")
                  do (define-key key-translation-map (kbd (concat prefix cpunc)) (kbd (concat prefix epunc)))))
