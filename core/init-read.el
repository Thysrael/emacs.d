;;; 翻译
;; (use-package google-translate
;;   :straight t
;;   :defines (google-translate-translation-directions-alist)
;;   :bind
;;   ("C-c j" . google-translate-at-point) ; 会询问一下是否是要查这个词
;;   ("C-c J" . google-translate-at-point-reverse)
;;   :init
;;   (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))) ; 用于设置 `smooth-translate`
;;
;; (use-package google-translate-default-ui
;;   :straight nil
;;   :custom
;;   (google-translate-default-source-language "en") ; 用于设置 `at-point-reverse`
;;   (google-translate-default-target-language "zh-CN"))

(use-package maple-translate
  :straight (maple-translate :type git :host github :repo "honmaple/emacs-maple-translate")
  :bind
  ("C-c j" . maple-translate+))

;;; 浏览器
;; 内置浏览器
(use-package eww
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface)
  (setq eww-retrieve-command '("readable") ; 只提取页面的可阅读部分
        shr-max-image-proportion 0.5)
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
  )

;; 将页面渲染成 org-mode
(use-package shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t
        ))

;; (use-package imenu-list
;;   :custom
;;   (imenu-list-position 'left)
;;   (imenu-list-size 0.2)
;;   :config
;;   ;; (setq imenu-list-focus-after-activation t)
;;   (setq imenu-list-auto-resize t)
;;   :bind
;;   ("C-c I" . imenu-list-smart-toggle)
;;   ("<tab>" . hs-toggle-hiding)
;;   ("TAB" . hs-toggle-hiding)
;;   )
