;; -*- lexical-binding: t; -*-

;; (use-package nerd-icons
;;   ;; :custom
;;   ;; The Nerd Font you want to use in GUI
;;   ;; "Symbols Nerd Font Mono" is the default and is recommended
;;   ;; but you can use any other Nerd Font if you want
;;   ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
;;   )

(use-package all-the-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; (use-package doom-modeline
;;   :hook 
;;   (after-init . doom-modeline-mode))


;; ;; How to detect the project root.
;; ;; nil means to use `default-directory'.
;; ;; The project management packages have some issues on detecting project root.
;; ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; ;; to hanle sub-projects.
;; ;; You can specify one if you encounter the issue.
;; (setq doom-modeline-project-detection 'auto)

;; ;; Whether display icons in the mode-line.
;; ;; While using the server mode in GUI, should set the value explicitly.
;; (setq doom-modeline-icon t)

;; ;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
;; (setq doom-modeline-major-mode-icon t)

;; ;; Whether display the colorful icon for `major-mode'.
;; ;; It respects `nerd-icons-color-icons'.
;; (setq doom-modeline-major-mode-color-icon t)

;; ;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
;; (setq doom-modeline-buffer-state-icon t)

;; ;; Whether display the modification icon for the buffer.
;; ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
;; (setq doom-modeline-buffer-modification-icon t)

;; ;; Whether display the time icon. It respects variable `doom-modeline-icon'.
;; (setq doom-modeline-time-icon t)

;; ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
;; (setq doom-modeline-unicode-fallback nil)

;; ;; Whether display the buffer name.
;; (setq doom-modeline-buffer-name t)

;; ;; Whether highlight the modified buffer name.
;; (setq doom-modeline-highlight-modified-buffer-name t)

;; ;; Whether display the minor modes in the mode-line.
;; (setq doom-modeline-minor-modes nil)

;; ;; If non-nil, a word count will be added to the selection-info modeline segment.
;; (setq doom-modeline-enable-word-count t)

;; ;; Major modes in which to display word count continuously.
;; ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
;; (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; ;; Whether display the buffer encoding.
;; (setq doom-modeline-buffer-encoding t)

;; ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
;; (setq doom-modeline-lsp t)

;; ;; Whether display the modal state.
;; ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
;; (setq doom-modeline-modal t)

;; ;; Whether display the modal state icon.
;; ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
;; (setq doom-modeline-modal-icon t)

;; ;; Whether display the modern icons for modals.
;; (setq doom-modeline-modal-modern-icon t)

;; ;; Whether display the battery status. It respects `display-battery-mode'.
;; (setq doom-modeline-battery nil)

;; (setq doom-modeline-always-show-macro-register t)

;; (setq column-number-mode t)

;; UI 性能优化
(setq
 idle-update-delay 1.0 ; 缓慢刷新 UI 界面
 redisplay-skip-fontification-on-input t ; 提高滚轮性能
 highlight-nonselected-windows nil ; 不高亮未选择窗口
 inhibit-compacting-font-caches t) ; 禁用压缩字体

;; 滚动
(setq
 fast-but-imprecise-scrolling t ; 滚动风格
 ; 滚动不会让光标过于靠上或者靠下（最多 5 行）
 scroll-step 0
 scroll-margin 5
 scroll-conservatively 101
 ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
 auto-window-vscroll nil
 ; 水平滚动
 auto-hscroll-mode t
 hscroll-step 0
 hscroll-margin 2)

(pixel-scroll-precision-mode) ; 像素级滚动

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

;; fringe 是 emacs window 侧边的“花边”
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)
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

;; 窗口分割线
(setq window-divider-default-places t
      window-divider-default-bottom-width 0 ; 底边依靠 modeline 进行分割，所以不用分割线
      window-divider-default-right-width 1) ; 右侧虽然有 fringe，依然有 1 个像素的分割线
(add-hook 'window-setup-hook #'window-divider-mode)

;; 像素级 resize，可以解决 emacs 最大化时无法填满的问题
(setq frame-resize-pixelwise t)

;; minibuffer 配置
; Allow minibuffer commands while in the minibuffer.
(setq enable-recursive-minibuffers t
      echo-keystrokes 0.02)
; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties '(read-only t
                                               intangible t
                                               cursor-intangible t
                                               face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
; Allow emacs to query passphrase through minibuffer
(setq epg-pinentry-mode 'loopback)

;; 字体，字体加载会花费大量时间
(defvar +font-en-size 20)
(defvar +font-han-size 20)
(defvar +font-emoji-size 18)

(defun +setup-fonts ()
  "Setup fonts."
  (set-face-attribute 'default nil :font (font-spec :family "JetBrainsMono Nerd Font" :size +font-en-size)) ; 设置英文字体
  (set-fontset-font t 'han (font-spec :family "LXGW WenKai" :size +font-han-size))
  (set-fontset-font t 'han (font-spec :script 'han) nil 'append) ; 设置中文字体 Sarasa Term SC LXGW WenKai
  )

(+setup-fonts)
(add-hook 'server-after-make-frame-hook #'+setup-fonts)

;; 光标
;; (blink-cursor-mode -1) ;; 阻止光标闪烁
(setq-default cursor-type 'bar)

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
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  )

;; (load-theme 'doom-ayu-mirage t)
(load-theme 'doom-moonlight t)
;; (load-theme'doom-nord t)
