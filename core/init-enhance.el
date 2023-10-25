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

;; 为 minibuffer 的选项提供更加详细的信息
(use-package marginalia
  :hook (vertico-mode . marginalia-mode))

;; 为 minibuffer 中出现的条目（不至于 minibuffer 条目）
(use-package embark
  :bind
  ("C-;" . embark-act)
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
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

  ;; ;; better preview
  ;; (consult-customize
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-bookmark consult-recent-file
  ;;  consult--source-recent-file
  ;;  consult--source-project-recent-file consult--source-bookmark
  ;;  consult-fd
  ;;  :preview-key "M-p")
  ;; (consult-customize
  ;;  consult-theme
  ;;  :preview-key (list "M-p" :debounce 0.6 'any))
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

;;; 光标移动增强
;; 字符跳转
(use-package avy
  :bind
  ("C-h" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.3) ; 0.3 秒后如果不连续击键，那么进入选择阶段
  (setq avy-background t)) ; 在跳转时背景变黑

;; 改变光标移动绑定，使其更加方便
(global-set-key (kbd "C-f") 'forward-word)
;; (global-set-key (kbd "C-b") 'backward-word)
;; (global-set-key (kbd "C-l") 'forward-char)
;; (global-set-key (kbd "C-h") 'backward-char)

;; 增强 C-e 使得其可以在关键位置进行循环移动
(use-package mosey
  :bind
  ("C-e" . mosey-forward-cycle))

;; [beginend] Better M-< M-> for programming
(use-package beginend
  :hook (after-init . beginend-global-mode))


;;; window 增强
;; 使用 M-n 快速切换窗口
(use-package ace-window
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  :bind
  ("C-x 9" . ace-delete-window)
  ("C-x 8" . ace-swap-window)
  :hook
  ((window-configuration-change . aw-update)) ;; For modeline
  :config
  (setq aw-scope 'frame
        aw-background nil
        aw-ignore-current t)

  ;; Select widnow via `M-1'...`M-9'
  (defun +aw--select-window (number)
    "Select the specified window."
    (let* ((window-list (aw-window-list))
           (target-window nil))
      (cl-loop for win in window-list
               when (and (window-live-p win)
                         (eq number
                             (string-to-number
                              (window-parameter win 'ace-window-path))))
               do (setq target-window win)
               finally return target-window)

      ;; Select the target window if found
      (if target-window
          (aw-switch-to-window target-window)
        (message "No specified window: %d" number))))

  (dotimes (n 9)
    (bind-key (concat "M-" (number-to-string (1+ n)))
              (lambda ()
                (interactive)
                (+aw--select-window (1+ n)))))
  )

;; 存储原来的窗口布局
(use-package winner
  :commands (winner-undo winner-redo)
  :init
  (setq winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode)
  :config
  (setq winner-boring-buffers
        '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
          "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
          "*esh command on file*"))
  )

;; popper 是一种特殊 buffer，所有的 popper 都只会占用同一个 window （出现在底部）
;; 利用这种机制可以使我们将许多临时的 buffer 都管理在一个窗口下
(use-package popper
  :bind
  ("C-=" . popper-toggle-type) ; 将 popper 转换为普通 buffer
  ("C--"  . popper-cycle) ; 切换多个 popper, 也可用于 toggle 出 popper window
  :hook
  (emacs-startup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*color-rg\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"
          "\\*Go-Translate\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          ibuffer-mode
          help-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode ag-mode pt-mode
          osx-dictionary-mode

          "^\\*Process List\\*" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode
          "^\\*eldoc.*\\*.*$" eldoc-mode

          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-mode

          chatgpt-shell-mode
	      ))

  :config
  ;; mode-line indicator
  (with-eval-after-load 'popper
    (setq popper-mode-line
          '(:eval `(:propertize " POP |"
                                face ,(+mode-line-get-window-name-face (+mode-line-window-active-p))))))

  ;; Enable indicator in minibuffer
  (popper-echo-mode 1)

  ;; HACK: close popper with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack)
  :custom
  (popper-window-height 35)
  )


;; [zoom] Managing the window sizes automatically
(use-package zoom)
