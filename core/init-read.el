;;; 翻译
(use-package google-translate
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
  (setq ;; eww-retrieve-command '("readable") ; 只提取页面的可阅读部分
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
                      (setq fill-column 110) ; 更宽的阅读视界
                      (setq truncate-lines nil) ; 自动折行
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
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t)
  :bind
  (:map shrface-mode-map
        ("n" . shrface-next-headline)
        ("p" . shrface-previous-headline)))
;; 设置代码字体，不知道为什么不能写进去
(custom-set-faces '(shrface-code ((t (:inherit org-code :family "JetBrainsMono Nerd Font")))))

(use-package shr-tag-pre-highlight
  :after shr
  :init
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (define-advice shr-tag-pre-highlight-guess-language-attr (:filter-return (&rest r) fallback-to-cpp)
    "如果检测不出来哪种语言，默认 C++."
    (or (car r) "c++"))
  )

;; (use-package eaf
;;   :straight nil
;;   :init
;;   (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;;   (require 'eaf)
;;   (setq eaf-config-location (no-littering-expand-var-file-name "eaf/"))
;;   (setq eaf-buffer-title-format "EAF: %s")
;;   )
;;
;; ;; 按 F 会有 avy 类似的效果
;; ;; 按 N 会将其转换为 eww 界面
;; (use-package eaf-browser
;;   :straight nil
;;   :init
;;   (require 'eaf-browser)
;;   (setq eaf-browser-dark-mode nil)
;;   (setq eaf-webengine-default-zoom 1.1)
;;   (setq eaf-webengine-font-size 22)
;;   (setq eaf-webengine-fixed-font-size 22)
;;   (setq eaf-webengine-pc-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15")
;;   :bind
;;   ("C-c Q" . eaf-open-browser-with-history)
;;   )
;;
;; (use-package eaf-pdf-viewer
;;   :straight nil
;;   :init
;;   (require 'eaf-pdf-viewer)
;;   (setq eaf-pdf-dark-mode nil)
;;   )


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
