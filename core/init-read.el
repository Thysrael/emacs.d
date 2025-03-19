;;; 翻译
;; (use-package google-translate
;;   :straight t
;;   :defines (google-translate-translation-directions-alist)
;;   :bind
;;   ("C-c j" . google-translate-at-point) ; 会询问一下是否是要查这个词
;;   ("C-c J" . google-translate-at-point-reverse)
;;   (:map org-mode-map
;;         ("C-," . nil)) ; conflict with sdcv
;;   :init
;;   (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))) ; 用于设置 `smooth-translate`
;;
;; (use-package google-translate-default-ui
;;   :straight nil
;;   :custom
;;   (google-translate-default-source-language "en") ; 用于设置 `at-point-reverse`
;;   (google-translate-default-target-language "zh-CN"))

;; (use-package maple-translate
;;   :straight (maple-translate :type git :host github :repo "honmaple/emacs-maple-translate")
;;   :bind
;;   ("C-c j" . maple-translate+)
;;   :custom
;;   (maple-translate-engine 'youdao))

;; 离线翻译
;; 需要安装 sdcv 和 stardict 和字典
;; 字典在 https://github.com/colordict/colordict.github.io/tree/master 可下载
(use-package sdcv
  :straight (:host github :repo "manateelazycat/sdcv")
  :commands (sdcv-search-pointer+)
  :bind ("C-," . sdcv-search-pointer+)
  :config
  (setq sdcv-say-word-p t)
  (setq sdcv-dictionary-data-dir "/usr/share/stardict")
  (setq sdcv-dictionary-simple-list
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"))
  (setq sdcv-dictionary-complete-list
        '("朗道英汉字典5.0"
          "牛津英汉双解美化版"
          "21世纪双语科技词典"
          "quick_eng-zh_CN"
          "新世纪英汉科技大词典"))
  (setq sdcv-tooltip-timeout 10)
  (setq sdcv-fail-notify-string "没找到释义")
  (setq sdcv-tooltip-border-width 0)
  :custom-face
  ;; TODO: why can't just inherit font-lock
  (sdcv-tooltip-face ((t (:background "#1E2029" :foreground "#ffc9e8"))))
  )

;;; 浏览器
;; 内置浏览器
(use-package eww
  ;; :init
  ;; (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  ;; (require 'shrface)
  (setq eww-retrieve-command '("readable") ; 只提取页面的可阅读部分
        shr-max-image-proportion 0.6)
  (defun +toggle-eww ()
    "Open eww if not already open, and switch to the *eww* buffer."
    (interactive)
    ;; Check if eww is already open
    (if (get-buffer "*eww*")
        ;; If *eww* buffer exists, switch to it
        (switch-to-buffer "*eww*")
      ;; If *eww* buffer doesn't exist, open eww
      (let ((url (read-string "Enter URL: ")))
        (eww url))))
  :hook
  (eww-mode . (lambda () (progn
                      (setq line-spacing 0.15) ; 行间距扩大
                      (setq fill-column 140) ; 更宽的阅读视界
                      (setq truncate-lines nil) ; 自动折行
                      (setq scroll-margin 2) ; 使滚动更平滑
                      )
                ))
  :bind
  ("C-c w" . +toggle-eww)
  ("C-c W" . eww-list-bookmarks)
  (:map eww-mode-map
        ("n" . next-line)
        ("p" . previous-line)
        ("f" . +smart-forward)
        ("b" . backward-char)
        ("a" . move-beginning-of-line)
        ("e" . move-end-of-line)
        ("o" . eww-toggle-images)
        ("v" . set-mark-command)
        ("q" . my-kill-region-or-line)
        ("w" . my-copy-region-or-line)
        ("," . sdcv-search-pointer+)
        ("l" . er/expand-region)
        ("L" . er/contract-region)
        ("g" . keyboard-quit)
        ("[" . eww-back-url)
        ("]" . eww-forward-url))
  )

;; ;; 将页面渲染成 org-mode
;; (use-package shrface
;;   :straight t
;;   :config
;;   (shrface-basic)
;;   (shrface-trial)
;;   (shrface-default-keybindings) ; setup default keybindings
;;   (setq shrface-href-versatile t)
;;   (setq shrface-toggle-bullets t)
;;   :bind
;;   (:map shrface-mode-map
;;         ;; ("n" . shrface-next-headline)
;;         ;; ("p" . shrface-previous-headline)
;;         ("C-c i" . shrface-headline-consult)))
;; ;; 设置代码字体，不知道为什么不能写进去
;; (custom-set-faces '(shrface-code ((t (:inherit org-code :family "JetBrainsMono Nerd Font")))))
;;
;; ;; 渲染 eww 内的代码高亮
;; (use-package shr-tag-pre-highlight
;;   :straight t
;;   :after shr
;;   :init
;;   (require 'shr-tag-pre-highlight)
;;   (add-to-list 'shr-external-rendering-functions
;;                '(pre . shr-tag-pre-highlight))
;;   (define-advice shr-tag-pre-highlight-guess-language-attr (:filter-return (&rest r) fallback-to-cpp)
;;     "如果检测不出来哪种语言，默认 C++."
;;     (or (car r) "c++"))
;;   )

(use-package image-mode
  :bind
  (:map image-mode-map
        ("=" . image-increase-size)
        ("-" . image-decrease-size)
        )
  )

;; (use-package eaf
;;   :straight nil
;;   :init
;;   (add-to-list 'load-path (expand-file-name "site-lisp/emacs-application-framework/" user-emacs-directory))
;;   (require 'eaf)
;;   (setq eaf-config-location (no-littering-expand-var-file-name "eaf/"))
;;   (setq eaf-buffer-title-format "EAF: %s")
;;   (setq eaf-kill-process-after-last-buffer-closed t)
;;   )

;; (use-package image-slicing
;;   :straight (image-slicing :type git :host github :repo "ginqi7/image-slicing")
;;   :config
;;   (require 'image-slicing)
;;   )


;; (use-package org-sliced-images
;;   :straight t
;;   :custom
;;   (org-sliced-images-round-image-height 40))

(use-package eaf
  :if (window-system)
  :straight (eaf
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app" "*.json")
             :includes (eaf-pdf-viewer) ; Straight won't try to search for these packages when we make further use-package invocations for them
             :pre-build (("python" "install-eaf.py" "--install" "pdf-viewer" "--ignore-sys-deps"))
             )
  :demand t
  :init
  (setq eaf-config-location (no-littering-expand-var-file-name "eaf/"))
  (setq eaf-buffer-title-format "EAF: %s")
  (setq eaf-kill-process-after-last-buffer-closed t)
  )

;; ;; 按 F 会有 avy 类似的效果
;; ;; 按 N 会将其转换为 eww 界面，不过最近还需要按 g 刷新才能正常显示
;; (use-package eaf-browser
;;   :straight nil
;;   :init
;;   (require 'eaf-browser)
;;   (setq eaf-browser-dark-mode "follow")
;;   (setq eaf-webengine-default-zoom 1.0)
;;   (setq eaf-webengine-font-size 24)
;;   (setq eaf-webengine-fixed-font-size 24)
;;   (setq eaf-webengine-pc-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15")
;;   :bind
;;   ("C-c Q" . eaf-open-browser-with-history)
;;   :config
;;   ;; 用 eaf-bind-key 进行快捷键绑定，同时不能直接绑定命令，要绑定去掉 `eaf-proxy` 后的命令
;;   (eaf-bind-key copy_text "C-w" eaf-browser-keybinding)
;;   (eaf-bind-key kill_text "C-q" eaf-browser-keybinding)
;;   (eaf-bind-key nil "C-d" eaf-browser-keybinding)
;;   (eaf-bind-key nil "C-t" eaf-browser-keybinding)
;;   (eaf-bind-key refresh_page "g" eaf-browser-keybinding)
;;   (eaf-bind-key insert_or_new_blank_page "s" eaf-browser-keybinding)
;;   (eaf-bind-key insert_or_scroll_up "n" eaf-browser-keybinding)
;;   (eaf-bind-key insert_or_toggle_device "p" eaf-browser-keybinding)
;;   )

;; M-h: add annotation
;; M-e: edit annotation
;; M-d: delete annotation
;; f: jump to link
;; o: outline
(use-package eaf-pdf-viewer
  :if (window-system)
  :demand t
  :init
  ;; (setq eaf-pdf-dark-mode "force")
  (setq eaf-pdf-dark-mode nil)
  :config
  (eaf-bind-key nil "i" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up "n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key eaf-pdf-outline "C-c o" eaf-pdf-viewer-keybinding)
  (eaf-bind-key gptel "d" eaf-pdf-viewer-keybinding)
  ;; (setq eaf-pdf-text-highlight-annot-color "#edd389")
  (setq eaf-pdf-inline-text-annot-fontsize 14)
  )

;; 安装后运行 pdf-tools-install 即可
(use-package pdf-tools
  :if (window-system)
  :straight t
  :config
  (pdf-tools-install :no-query))

;; (use-package nov
;;   :straight t
;;   :bind
;;   (:map nov-mode-map
;;         ("n" . next-line)
;;         ("p" . previous-line)
;;         ("f" . +smart-forward-cn)
;;         ("b" . backward-char)
;;         ("a" . mwim-beginning-of-code-or-line)
;;         ("e" . mwim-end-of-code-or-line)
;;         ("v" . set-mark-command)
;;         ("g" . keyboard-quit))
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; n, p: next, before iamge
;; j, k, h, l: move focus
;; -, =. 0: in, out, reset
;; y, u, i, o: flip, rotate
;; (use-package eaf-image-viewer
;;   :straight nil
;;   :init
;;   (require 'eaf-image-viewer))

;; (use-package imenu-list
;;   :after imenu
;;   :custom
;;   (imenu-list-position 'left)
;;   (imenu-list-size 0.2)
;;   :config
;;   (setq imenu-list-focus-after-activation t)
;;   (setq imenu-list-auto-resize t)
;;   :bind
;;   ("C-c I" . imenu-list-smart-toggle)
;;   ("<tab>" . hs-toggle-hiding)
;;   ("TAB" . hs-toggle-hiding)
;;   )

;; ;; 需要执行 `yay -S emacs-pdf-tools-git` 才可以正常使用
;; (use-package pdf-tools
;;   :straight t
;;   :defines pdf-annot-activate-created-annotations
;;   :hook ((pdf-tools-enabled . pdf-view-auto-slice-minor-mode))
;;   :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
;;   :bind (:map pdf-view-mode-map
;;          ([remap scroll-up-command] . pdf-view-scroll-up-or-next-page)
;;          ([remap scroll-down-command] . pdf-view-scroll-down-or-previous-page))
;;   :init
;;   (setq pdf-view-use-scaling t
;;         pdf-view-use-imagemagick nil
;;         pdf-annot-activate-created-annotations t)
;;   (setq pdf-sync-backward-display-action t
;;       pdf-sync-forward-display-action t)
;;   :config
;;   (pdf-tools-install t nil t nil)
;;   )
;;
;; (use-package pdf-isearch
;;   :after pdf-tools
;;   :hook (pdf-tools-enabled . pdf-isearch-minor-mode))
;;
;; (use-package pdf-outline
;;   :after pdf-tools
;;   :hook (pdf-tools-enabled . pdf-outline-minor-mode))
;;
;; ;; [saveplace-pdf-view] Recover last viewed position
;; (use-package saveplace-pdf-view
;;   :straight t
;;   :when (ignore-errors (pdf-info-check-epdfinfo) t)
;;   :autoload (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
;;   :init
;;   (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
;;   (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))
