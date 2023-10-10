;;; 临时文件设置
(setq-default
 ;; 关闭锁文件，防止生成 file~#
 create-lockfiles nil
 ;; 关闭备份文件，防止生成 file~
 make-backup-files nil
 ;; 自动保存功能，#file#
 auto-save-default t ; 开启自动保存功能 
 auto-save-include-big-deletions t  ; 自动保存较大的删除
 auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "autosaves/") t)) ; 指定保存位置
 auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                            (concat auto-save-list-file-prefix "tramp-\\2") t)
                                      (list ".*" auto-save-list-file-prefix t)) ; 避免 tramp 和 local 的冲突
 )
;; 设置自动恢复数据
(advice-add #'after-find-file :around
            (lambda (fn &rest args) (cl-letf (((symbol-function #'sit-for) #'ignore))
                                      (apply fn args))))


;;; 过长文本的显示问题
;; 折行和截断
(setq-default 
 word-wrap t ; 会按照单词进行折行
 word-wrap-by-category t ; 对中文折行的支持
 fill-column 80 ; 设置 fill-column 为 80，这个变量会影响其他变量
 truncate-lines t ; truncate 会将超出部分的文本截断
 truncate-partial-width-windows nil
 truncate-string-ellipsis "..." ; 文本截断的省略符为 ...
 )

;; 开启视觉折行
(use-package visual-line-mode
  :straight nil ; 不要尝试从包管理器中安装 visual-line-mode
  :hook (text-mode . visual-line-mode) ; 将 visual-line-mode 应用于 text 模式
  ) 

;; 用于单行长文件
(use-package so-long
  :hook 
  (after-init . global-so-long-mode)
  :config
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil)) ; saveplace 不会对其使用
  )


;;; 用户界面基础设置
(setq-default 
 ;; 没有客户端启动信息
 server-client-instructions nil

 ;; 禁止双向文本（类似阿拉伯语或者希伯来语）
 bidi-inhibit-bpa t
 bidi-paragraph-direction 'left-to-right
 bidi-display-reordering 'left-to-right

 ;; 关闭响铃
 ring-bell-function 'ignore

 ;; 缩进设置
 tabify-regexp "^\t* [ \t]+" ; 将缩进用的空格转变成制表符的命令
 indent-tabs-mode nil ; 使用空格而不是制表符
 tab-always-indent t
 tab-width 4 ; 默认宽度为 4

 ;; 用 y-or-n to 代替 yes-or-no
 use-short-answers t

 ;; 避免文件重名警告
 find-file-suppress-same-file-warnings t

 ;; 文件在最后一行之后会自动添加一个换行符，符合 POSIX 规范
 require-final-newline t

 ;;避免在创建文件时提示不存在文件
 confirm-nonexistent-file-or-buffer nil

 ;; 如果 buffer 名字相同，则会显示 path/name
 uniquify-buffer-name-style 'forward
 )

;;; 基础键位绑定
;; 强化复制和粘贴功能
(defun my-kill-region-or-line ()
  "Kill the region or the current line if no region is active, and keep the cursor at its original position."
  (interactive)
  (let ((start (point)))
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (kill-whole-line))
    (goto-char start)))

(defun my-copy-region-or-line ()
  "Save the region to the kill ring or the current line if no region is active, and keep the cursor at its original position."
  (interactive)
  (let ((start (point)))
    (if (region-active-p)
        (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (line-beginning-position) (line-end-position)))
    (goto-char start)))

;; 绑定新的复制和剪切函数
(global-set-key (kbd "C-q") 'my-kill-region-or-line)
(global-set-key (kbd "C-w") 'my-copy-region-or-line)

;; 模仿 CUA 模式
(global-set-key (kbd "C-S-v") 'yank)
(global-set-key (kbd "C-S-x") 'my-kill-region-or-line)
(global-set-key (kbd "C-S-c") 'my-copy-region-or-line)

;; 将关键命令移动到右手核心区
(define-key key-translation-map (kbd "C-j") (kbd "C-x"))
(global-set-key (kbd "C-k") 'execute-extended-command)

;; 绑定恢复 buffer 函数
(global-set-key (kbd "<f5>") 'revert-buffer) ; 撤销所有没有保存的更改
(global-set-key (kbd "<f6>") 'recover-file) ; 利用 autosave 完成更新

;; window scroll 绑定
(defvar +scrolling-lines 5)
(bind-keys*
 ;; 在其他窗口滚动
 ("M-<down>" . (lambda () (interactive) (scroll-other-window +scrolling-lines)))
 ("M-<up>" . (lambda () (interactive) (scroll-other-window (- +scrolling-lines))))
 ;; 在本窗口滚动
 ("C-v" . (lambda () (interactive) (scroll-up +scrolling-lines)))
 ("M-v" . (lambda () (interactive) (scroll-up (- +scrolling-lines)))))

;;; 历史信息记录
;; save-place-mode 可以保存文件的上次浏览位置，即使 emacs 关闭也可以保存
(use-package saveplace
  :hook 
  (after-init . save-place-mode)
  :config
  ;; HACK: `save-place-alist-to-file' uses `pp' to prettify the contents of its cache, which is expensive and useless.
  ;; replace it with `prin1'
  (advice-add #'save-place-alist-to-file :around
              (lambda (fn) (cl-letf (((symbol-function #'pp) #'prin1))
                             (funcall fn))))
  )

;; 调用最近的浏览文件记录
(use-package recentf
  :bind 
  (("C-x C-r" . recentf-open-files))
  :hook 
  (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never ; 不自动清理
        recentf-max-saved-items 200 ; 最大记录数是 200
        recentf-keep nil) ; 不会刻意保留任何文件

  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory)) ; ~/.emacs/var 排除
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory)) ; ~/.emacs/etc 排除

  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name) ; 展示时使用相对路径
  (add-to-list 'recentf-filename-handlers #'substring-no-properties) ; 移除文件名中的文本属性
  (add-hook 'dired-mode-hook (lambda () (recentf-add-file default-directory))) ; 加入 dired
  )

;; 用于保存和恢复 Emacs 会话期间的用户交互历史记录，包括命令历史、minibuffer 历史、搜索和替换历史等。
(use-package savehist
  :hook 
  (after-init . savehist-mode)
  :config
  (setq savehist-additional-variables '(mark-ring global-mark-ring
                                                  search-ring regexp-search-ring
                                                  kill-ring) ; 要保存的列表
        savehist-autosave-interval 300) ; 每 300s 保存一次
                                        ; 移除了 kill-ring 中的文本属性，以减小 savehist 缓存的大小  
  (add-hook 'savehist-save-hook
            (lambda () (setq kill-ring
                             (mapcar #'substring-no-properties
                                     (cl-remove-if-not #'stringp kill-ring))
                             register-alist
                             (cl-loop for (reg . item) in register-alist
                                      if (stringp item)
                                      collect (cons reg (substring-no-properties item))
                                      else collect (cons reg item)))))
  (with-eval-after-load 'vertico
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)) ; 记录 vertico repeat 信息
  )


;;; misc 杂项设置
;; setq-default 中的设置会对所有的 buffer 生效
(setq-default
 ;; 使得读取外部程序的输出增大，可以提高效率
 read-process-output-max (* 3 1024 1024)

 ;; 对于符号链接永远直接跳转
 find-file-visit-truename t
 vc-follow-symlinks t

 ;; 在补全的时候忽略大小写
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t

 ;; 设置句子结尾
 sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil

 ;; emacs 不会自动解析域名
 ffap-machine-p-known 'reject
 )

;; 设置编码方式
(set-language-environment "UTF-8")

;; 优化垃圾回收
(use-package gcmh
  :hook 
  (emacs-startup . gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x64000000)
  )
