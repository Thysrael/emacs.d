;; (use-package holo-layer
;;   :straight (holo-layer :type git :host github :repo "manateelazycat/holo-layer"))

;; 果冻光标
;;(add-to-list 'load-path "~/.emacs.d/straight/repos/holo-layer")
;;(require 'holo-layer)
;;(setq holo-layer-enable-cursor-animation t)
;;(holo-layer-enable)

;; (use-package key-echo
;;   :straight (key-echo :type git :host github :repo "manateelazycat/key-echo"))

;; (add-to-list 'load-path "~/.emacs.d/straight/repos/key-echo/")
;; (require 'key-echo)
;; (setq key-echo-enable-debug t)
;;
;;
;; (defun key-echo-shift-to-switch-input-method (key)
;;   (interactive)
;;   (when (string-equal key "Key.shift")
;;     (toggle-input-method)
;;     ))
;;
;; (setq key-echo-single-key-trigger-func 'key-echo-shift-to-switch-input-method)
;;
;; (key-echo-enable)

;; (defun popper-display-in-posframe (buf _)
;;   (when (posframe-workable-p)
;;   (posframe-show buf
;;                  :position t
;;                  :poshandler #'posframe-poshandler-frame-center
;;                  :width 72
;;                  :height 25
;;                  :border-width 3
;;                  :border-color "IndianRed")))
;;
;; (setq popper-display-function #'popper-display-in-posframe)

;; (use-package tabspaces
;;   ;; use this next line only if you also use straight, otherwise ignore it.
;;   :straight (:type git :host github :repo "mclear-tools/tabspaces")
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   ;; sessions
;;   (tabspaces-session t))

(use-package google-translate
  :straight t
  :defines (google-translate-translation-directions-alist)
  :bind
  ("C-c j" . google-translate-smooth-translate) ; 会询问一下是否是要查这个词
  ("C-c J" . google-translate-at-point-reverse)
  :init
  (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))) ; 用于设置 `smooth-translate`

(use-package google-translate-default-ui
  :straight nil
  :custom
  (google-translate-default-source-language "en") ; 用于设置 `at-point-reverse`
  (google-translate-default-target-language "zh-CN"))
