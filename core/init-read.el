;;; 翻译
(use-package google-translate
  :straight t
  :defines (google-translate-translation-directions-alist)
  :bind
  ("C-c j" . google-translate-at-point) ; 会询问一下是否是要查这个词
  ("C-c J" . google-translate-at-point-reverse)
  :init
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))) ; 用于设置 `smooth-translate`

(use-package google-translate-default-ui
  :straight nil
  :custom
  (google-translate-default-source-language "en") ; 用于设置 `at-point-reverse`
  (google-translate-default-target-language "zh-CN"))

;; (use-package maple-translate
;;   :straight (maple-translate :type git :host github :repo "honmaple/emacs-maple-translate")
;;   :bind
;;   ("C-c j" . maple-translate+)
;;   :custom
;;   (maple-translate-engine 'youdao))

;;; 浏览器
;; 内置浏览器
(use-package eww
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface)
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
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . +smart-forward)
        ("h" . backward-char)
        ("i" . eww-toggle-images)
        ("o" . maple-translate)
        ("m" . set-mark-command)
        ("," . eww-back-url)
        ("." . eww-forward-url))
  )

;; 将页面渲染成 org-mode
(use-package shrface
  :straight t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t)
  (setq shrface-toggle-bullets t)
  :bind
  (:map shrface-mode-map
        ("n" . shrface-next-headline)
        ("p" . shrface-previous-headline)
        ("C-c i" . shrface-headline-consult)))
;; 设置代码字体，不知道为什么不能写进去
(custom-set-faces '(shrface-code ((t (:inherit org-code :family "JetBrainsMono Nerd Font")))))

(use-package shr-tag-pre-highlight
  :straight t
  :after shr
  :init
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (define-advice shr-tag-pre-highlight-guess-language-attr (:filter-return (&rest r) fallback-to-cpp)
    "如果检测不出来哪种语言，默认 C++."
    (or (car r) "c++"))
  )

(use-package eaf
  :straight nil
  :init
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)
  (setq eaf-config-location (no-littering-expand-var-file-name "eaf/"))
  (setq eaf-buffer-title-format "EAF: %s")
  (setq eaf-kill-process-after-last-buffer-closed t)
  )

;; 按 F 会有 avy 类似的效果
;; 按 N 会将其转换为 eww 界面，不过最近还需要按 g 刷新才能正常显示
(use-package eaf-browser
  :straight nil
  :init
  (require 'eaf-browser)
  (setq eaf-browser-dark-mode "follow")
  (setq eaf-webengine-default-zoom 1.0)
  (setq eaf-webengine-font-size 24)
  (setq eaf-webengine-fixed-font-size 24)
  (setq eaf-webengine-pc-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15")
  :bind
  ("C-c Q" . eaf-open-browser-with-history)
  :config
  ;; 用 eaf-bind-key 进行快捷键绑定，同时不能直接绑定命令，要绑定去掉 `eaf-proxy` 后的命令
  (eaf-bind-key copy_text "C-w" eaf-browser-keybinding)
  (eaf-bind-key kill_text "C-q" eaf-browser-keybinding)
  (eaf-bind-key nil "C-d" eaf-browser-keybinding)
  (eaf-bind-key nil "C-t" eaf-browser-keybinding)
  (eaf-bind-key refresh_page "g" eaf-browser-keybinding)
  (eaf-bind-key insert_or_new_blank_page "s" eaf-browser-keybinding)
  (eaf-bind-key insert_or_scroll_up "n" eaf-browser-keybinding)
  (eaf-bind-key insert_or_toggle_device "p" eaf-browser-keybinding)
  )

(use-package eaf-pdf-viewer
  :straight nil
  :init
  (require 'eaf-pdf-viewer)
  (setq eaf-pdf-dark-mode "follow")
  )

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
