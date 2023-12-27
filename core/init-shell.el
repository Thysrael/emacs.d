;; 配置 eshell
(use-package esh-mode
  :straight nil
  :functions eshell/alias
  :custom-face
  ;; 避免路径下划线
  (eshell-syntax-highlighting-file-arg-face ((t (:underline nil))))
  :custom
  ;; 限制 eshell 全屏弹出
  (display-comint-buffer-action '(display-buffer-at-bottom
                                         (inhibit-same-window . nil)))
  :bind (("C-t" . project-eshell)
         ("C-c t" . eshell)
         :map eshell-mode-map
         ("C-l" . eshell/clear)
         ("C-c C-h" . consult-history)
         ("M-b" . backward-kill-word)
         ("C-e" . end-of-line)
         ("C-a" . (lambda ()
                    (interactive)
                    (beginning-of-line)
                    (search-forward-regexp eshell-prompt-regexp))))
  :config
  (setq
   eshell-banner-message "" ; 禁用起始信息

   ;; 输入和输出都会让 eshell 滚动
   eshell-scroll-to-bottom-on-input 'all
   eshell-scroll-to-bottom-on-output 'all

   ;; exit
   eshell-kill-processes-on-exit t
   eshell-hist-ignoredups t

   eshell-input-filter #'eshell-input-filter-initial-space

   ;; eshell 通配符 (glob) 设置
   eshell-glob-case-insensitive t ; 大小写不敏感
   eshell-error-if-no-glob t ; more like zsh

   ;; prefer eshell functions
   eshell-prefer-lisp-functions t

   ;; 有些命令 eshell 无能为力，只能退而求其次
   eshell-visual-commands '("top" "htop" "bat" "talnet")
   eshell-visual-subcommands '(("git" "help" "lg" "log" "diff" "show"))

   eshell-cmpl-ignore-case t ; 补全时忽略大小写
   eshell-cmpl-cycle-completions t ; 循环补全
   )

  ;; [UI]
  (add-hook 'eshell-mode-hook
            (defun +eshell--set-window-ui-h ()
              (set-window-fringes nil 0 0)
              (set-window-margins nil 1 nil)
              (visual-line-mode +1)
              (set-display-table-slot standard-display-table 0 ?\ )))

  (defun +eshell/define-alias ()
    "Define alias for eshell"
    ;; Aliases
    (defalias 'eshell-f 'find-file)
    (defalias 'eshell-fo 'find-file-other-window)
    (defalias 'eshell-d 'dired)
    (eshell/alias "ll" "ls -laG $*")
    (defalias 'eshell-q 'eshell/exit)
    (eshell/alias "rg" "rg --color=always $*")
    (defalias 'eshell-clear 'eshell/clear-scrollback)
    ;; Git
    (eshell/alias "ga" "git add .")
    (eshell/alias "gc" "git commit -m")
    )
  (add-hook 'eshell-first-time-mode-hook #'+eshell/define-alias)
  ;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file' via elisp
  (advice-add #'eshell-write-aliases-list :override #'ignore)

  ;; A bunch of eshell functions
  ;; 这些命令以 `ehsell/` 开头，可以在 eshell 中省略前缀调用
  ;; [clear]
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  ;; [bat] 可以高亮代码
  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks (set-auto-mode) (font-lock-ensure)))
      (buffer-string)))
  ;; [bd]
  (defun eshell/bd ()
    "cd to parent directory with completions."
    (let ((dir default-directory)
          dirs)
      (while (not (string-empty-p dir))
        (push (file-name-directory dir) dirs)
        (setq dir (substring dir 0 -1)))
      (let ((dir (completing-read "Directory: " dirs nil t)))
        (eshell/cd dir))))
  )

;; 支持未完成命令和非法命令高亮
(use-package eshell-syntax-highlighting
  :after eshell
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

;; 自动提示
(use-package capf-autosuggest
  :after eshell
  :hook
  (eshell-mode . capf-autosuggest-mode)
  :bind
  (:map capf-autosuggest-active-mode-map
        ("C-e" . capf-autosuggest-accept)
        ("<right>" . capf-autosuggest-accept)))

;; 输入 z 命令后会弹出 minibuffer 选择 path
(use-package eshell-z
  :after eshell
  :commands (eshell/z))

;; promter 自定义
(use-package eshell-git-prompt
  :after eshell
  :init
  (require 'eshell-git-prompt)
  :config
  (defun curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (when (and (eshell-search-path "git")
               (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
        (if (> (length git-output) 0)
            (substring git-output 0 -1)
          "(no branch)"))))

  (defun pwd-replace-home (pwd)
    "Replace home in PWD with tilde (~) character."
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
      (if (and
           (>= (length pwd) home-len)
           (equal home (substring pwd 0 home-len)))
          (concat "~" (substring pwd home-len))
        pwd)))

  (defun split-directory-prompt (directory)
    "Break up the directory into a 'parent' and a 'base'"
    (if (string-match-p ".*/.*" directory)
        (list (file-name-directory directory) (file-name-base directory))
      (list "" directory)))
  ;; Create a custom prompt using the functions above
  (setq eshell-prompt-function
        (lambda ()
          (let* ((directory (split-directory-prompt (pwd-replace-home (eshell/pwd))))
                 (parent (car directory))
                 (name (cadr directory))
                 (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")))
            (concat   ;; Prompt for Dark Themes
             (propertize "┌─[" 'face `(:inherit font-lock-keyword-face))
             (propertize parent 'face `(:inherit eshell-prompt))
             (propertize name   'face `(:inherit eshell-prompt :weight bold))
             (propertize "]" 'face `(:inherit font-lock-keyword-face))
             (if (string-empty-p branch)
                 ""
               (propertize "──[" 'face `(:inherit font-lock-keyword-face)))
             (propertize branch 'face `(:inherit font-lock-variable-name-face))
             (if (string-empty-p branch)
                 ""
               (propertize "]" 'face `(:inherit font-lock-keyword-face)))
             ;; git status 信息
             (-when-let (git-status (eshell-git-prompt--collect-status))
               (-let [(&plist :untracked untracked
                              :new-added new-added
                              :modified-updated modified-updated
                              :modified modified
                              :deleted deleted
                              :deleted-updated deleted-updated
                              :renamed-updated renamed-updated)
                      git-status]
                 (concat
                  (let (work-info)
                    (setq work-info
                          (concat
                           ;; new add file
                           (when (> untracked 0)
                             (concat
                              (number-to-string untracked)
                              (propertize "? " 'face `(:foreground "#77e0c6" :weight bold))
                              ))
                           ;; modified file
                           (when (> modified 0)
                             (concat
                              (number-to-string modified)
                              (propertize "* " 'face `(:foreground "#82aaff" :weight bold))
                              ))
                           ;; deleted file
                           (when (> deleted 0)
                             (concat
                              (number-to-string deleted)
                              (propertize "x " 'face `(:foreground "#ff757f" :weight bold))
                              ))
                           )
                          )
                    (when (> (length work-info) 0)
                      (concat
                       (propertize "─[u: " 'face `(:inherit font-lock-keyword-face))
                       (substring work-info 0 (- (length work-info) 1))
                       (propertize "]" 'face `(:inherit font-lock-keyword-face))
                       )))
                  ;; Updated to index changes
                  (let (commit-info)
                    (setq commit-info
                          (concat
                           ;; new add file
                           (when (> new-added 0)
                             (concat
                              (number-to-string new-added)
                              (propertize "+ " 'face `(:foreground "#77e0c6" :weight bold))
                              ))
                           ;; modified file
                           (when (> modified-updated 0)
                             (concat
                              (number-to-string modified-updated)
                              (propertize "* " 'face `(:foreground "#82aaff" :weight bold))
                              ))
                           ;; deleted file
                           (when (> deleted-updated 0)
                             (concat
                              (number-to-string deleted-updated)
                              (propertize "x " 'face `(:foreground "#ff757f" :weight bold))
                              ))
                           ;; renamed file
                           (when (> renamed-updated 0)
                             (concat
                              (number-to-string renamed-updated)
                              (propertize "r " 'face `(:foreground "#ffb86c" :weight bold))
                              ))
                           )
                          )
                    (when (> (length commit-info) 0)
                      (concat
                       (propertize "─[s: " 'face `(:inherit font-lock-keyword-face))
                       (substring commit-info 0 (- (length commit-info) 1))
                       (propertize "]" 'face `(:inherit font-lock-keyword-face))
                       )))
                  )))
             (propertize "\n" 'face `(:inherit font-lock-keyword-face))
             (propertize "└─>" 'face `(:inherit font-lock-keyword-face))
             (propertize " λ "   'face `(:inherit font-lock-function-call-face :weight ultra-bold))
             )
            )))
  ;; 这个正则需要匹配输入行，只有配置对了 eshell 功能才对
  (setq eshell-prompt-regexp "└─> λ "))
