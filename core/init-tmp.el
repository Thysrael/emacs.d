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


;; [timeout] debounce and throttle
(use-package timeout
  :straight (:host github :repo "karthink/timeout" :branch "master"))

;; [zoom] Managing the window sizes automatically
;; (use-package zoom
;;   :hook (window-setup . zoom-mode)
;;   :config
;;   (timeout-throttle! 'zoom--handler 0.1))

;; (defun +patch/eglot-pyright-venv-workspace-config (server)
;;   `(:python\.analysis
;;     (:extraPaths ,(vector "~/learn/sem7/Isolation/gem5/src/python/"))))
;;
;; (setq-default eglot-workspace-configuration #'+patch/eglot-pyright-venv-workspace-config)

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  (setq chatgpt-shell-prompt-query-response-style #'shell)
  (setq chatgpt-shell-prompt-header-describe-code "What does the following code do? Use chinese to answer it")
  (setq chatgpt-shell-model-version 4)
  :bind
  ("C-c q" . chatgpt-shell-explain-code)
  ;; ("C-c q" . chatgpt-shell)
  ("C-d" . chatgpt-shell)
  ;; M-n/p 可以查询历史
  (:map chatgpt-shell-mode-map
        ("M-<return>" . chatgpt-shell-newline)
        ("<return>" . chatgpt-shell-submit)))
