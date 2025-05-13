;; -*- lexical-binding: t; -*-

(use-package nerd-icons
  :ensure t
  :custom
  ;; 必须使用这个字体，字体显示才完全
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; (nerd-icons-scale-factor 1.1)
  :config
  ;; push 比 add-to-list 更高效，因为 add-to-list 要扫描整个 list 确保不重复
  ;; 要指定后缀名、字体集、face
  (push '("drawio" nerd-icons-mdicon "nf-md-drawing" :face nerd-icons-lpink) nerd-icons-extension-icon-alist)
  (push '("rst" nerd-icons-mdicon "nf-md-file_document" :face nerd-icons-lpink) nerd-icons-extension-icon-alist)

  (push '(conf-space-mode nerd-icons-codicon "nf-cod-settings" :face nerd-icons-lyellow) nerd-icons-mode-icon-alist)
  (push '(pdf-outline-buffer-mode nerd-icons-mdicon "nf-md-view_list" :face nerd-icons-dred) nerd-icons-mode-icon-alist)
  (push '(pdf-occur-buffer-mode nerd-icons-mdicon "nf-md-view_list" :face nerd-icons-dred) nerd-icons-mode-icon-alist)
  ;; regexp 是在文件没有拓展名时，采用的匹配策略
  (push '("^config$" nerd-icons-codicon "nf-cod-settings" :face nerd-icons-lyellow) nerd-icons-regexp-icon-alist)
  ;; (push '("rc$" nerd-icons-codicon "nf-cod-settings" :face nerd-icons-lyellow) nerd-icons-regexp-icon-alist)
  )

;; treemacs needed
;; (use-package all-the-icons
;;   :straight t
;;   ;; :custom
;;   ;; The Nerd Font you want to use in GUI
;;   ;; "Symbols Nerd Font Mono" is the default and is recommended
;;   ;; but you can use any other Nerd Font if you want
;;   ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
;;   )

;; UI 性能优化
(setq
 idle-update-delay 1.0 ; 缓慢刷新 UI 界面
 redisplay-skip-fontification-on-input t ; 提高滚轮性能
 highlight-nonselected-windows nil ; 不高亮未选择窗口
 inhibit-compacting-font-caches t) ; 禁用压缩字体

;; 滚动
(setq
 fast-but-imprecise-scrolling t ; 滚动风格
 ;; 滚动不会让光标过于靠上或者靠下（最多 5 行）
 scroll-step 0
 scroll-margin 5
 scroll-conservatively 101
 ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
 auto-window-vscroll nil
 ;; 水平滚动
 auto-hscroll-mode t
 hscroll-step 0
 hscroll-margin 2)

(pixel-scroll-precision-mode) ; 像素级滚动

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))


(when (display-graphic-p)
  ;; fringe 是 emacs window 侧边的“花边”
  ;; make left-fringe half
  (fringe-mode '(5 . 8))
  ;; Better fringe symbol
  (define-fringe-bitmap 'right-curly-arrow
    [#b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000])
  (define-fringe-bitmap 'right-arrow
    [#b00000000
     #b00000000
     #b00001110
     #b00001110
     #b00001110
     #b00000000
     #b00000000
     #b00000000])
  (define-fringe-bitmap 'left-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b01110000
     #b01110000
     #b01110000
     #b00000000
     #b00000000])
  ;; 字体，字体加载会花费大量时间
  (defvar +font-en-size)
  (defvar +font-han-size)
  (defvar +font-emoji-size)

  (let ((hostname (system-name)))
    (if (string-equal hostname "banana")
        (progn
          ;; 当主机名为 banana 时
          (setq +font-en-size 28)
          (setq +font-han-size 28)
          (setq +font-emoji-size 28))
      (progn
        ;; 当主机名不是 banana 时
        (setq +font-en-size 18)
        (setq +font-han-size 18)
        (setq +font-emoji-size 18))))

  (defun +setup-fonts ()
    "Setup fonts."
    ;; JetBrainsMono 这个字体并不支持一些字符，好像 Sarasa Term SC 支持得更多一些，但是不知道 vscode 为啥可以
    (set-face-attribute 'default nil :font (font-spec :family "JetBrainsMono" :size +font-en-size)) ; 设置英文字体
    (set-fontset-font t 'han (font-spec :family "TsangerJinKai05" :size +font-han-size))
    (set-fontset-font t 'han (font-spec :script 'han) nil 'append) ; forbidden use backup font
    (set-fontset-font t 'cjk-misc (font-spec :family "Sarasa Mono SC" :size +font-han-size)) ; 设置全角标点，如果是仓耳有点丑，是半角的
    (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono") nil 'append)
    ;; (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'append)
    )

  ;; (defun +setup-fonts ()
  ;;     "Setup fonts."
  ;;     (set-face-attribute 'default nil :font (font-spec :family "Sarasa Term SC" :size +font-en-size))
  ;;     (set-face-font 'fixed-pitch "Sarasa Term SC")
  ;;     (set-face-font 'fixed-pitch-serif "Sarasa Term Slab SC")
  ;;     (set-face-font 'variable-pitch "Sarasa UI SC")
  ;;
  ;;     (dolist (charset '(han cjk-misc))
  ;;       (set-fontset-font t charset (font-spec :family "LXGW WenKai" :size +font-han-size)))
  ;;     (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'append)
  ;;     )

  (+setup-fonts)

  (add-hook 'server-after-make-frame-hook #'+setup-fonts)  
  )
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; 窗口分割线
(setq window-divider-default-places t
      window-divider-default-bottom-width 0 ; 底边依靠 modeline 进行分割，所以不用分割线
      window-divider-default-right-width 1) ; 右侧虽然有 fringe，依然有 1 个像素的分割线
(add-hook 'window-setup-hook #'window-divider-mode)

;; 像素级 resize，可以解决 emacs 最大化时无法填满的问题
(setq frame-resize-pixelwise t)

;; 改变透明度
(add-to-list 'default-frame-alist '(alpha-background . 98))

;; minibuffer 配置
;; Allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t
      echo-keystrokes 0.02)
;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties '(read-only t
                                               intangible t
                                               cursor-intangible t
                                               face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; Allow emacs to query passphrase through minibuffer
(setq epg-pinentry-mode 'loopback)

;; 光标
;; (blink-cursor-mode -1) ;; 阻止光标闪烁
;; (setq-default cursor-type 'bar)

;; 连体字
;; (use-package ligature
;;   :hook
;;   ((prog-mode markdown-mode) . ligature-mode)
;;   :config
;;   ;; Enable all Cascadia Code ligatures in programming modes
;;   (ligature-set-ligatures '(prog-mode markdown-mode org-mode)
;;                           '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
;;                             ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
;;                             "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
;;                             "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
;;                             "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
;;                             "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
;;                             "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
;;                             "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
;;                             ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
;;                             "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
;;                             "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
;;                             "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
;;                             "\\\\" "://"))
;;   )

;; frame 标题
(setq frame-title-format
      '((:eval (or buffer-file-truename "%b"))
        (" · Emacs")))

;; 初始化全屏
(setq initial-frame-alist '((fullscreen . maximized)))

;; 主题
(setq custom-safe-themes t)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :init
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  )

;; (use-package modus-themes)
;; (load-theme 'doom-ayu-mirage t)
;; (load-theme 'doom-moonlight t)
(load-theme 'doom-dracula t)
;; (load-theme'doom-nord t)
;; (load-theme 'modus-vivendi t)
;; (load-theme 'doom-zenburn t)

;; (use-package ef-themes)

(custom-set-faces
 ;; 对代码高亮进行微调
 ;; unspecified 就是忽略的意思
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face :slant italic))))
 '(font-lock-number-face ((t (:foreground unspecified :inherit (font-lock-constant-face)))))
 '(font-lock-property-name-face ((t (:inherit font-lock-variable-name-face :slant italic))))
 '(font-lock-property-use-face ((t (:inherit font-lock-value-use-face :slant italic))))
 '(font-lock-variable-use-face ((t (:inherit unspecified))))
 '(font-lock-preprocessor-face ((t (:foreground unspecified :family "JetBrainsMono"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground unspecified))))
 ;; 设置变宽字体和等宽字体，但是不知道为啥不能放到 UI 里
 '(fixed-pitch ((t (:family "JetBrainsMono"))))
 '(variable-pitch ((t (:height 0.9 :family "Symbols Nerd Font Mono")))) ;; SourceHanSerifCN
 ;; 设置加粗字体
 '(bold ((t (:inherit (font-lock-builtin-face) :weight ultra-bold))))
 ;; '(bold ((t (:inherit (font-lock-builtin-face) :weight ultra-bold :family "Sarasa Mono SC"))))
 )
