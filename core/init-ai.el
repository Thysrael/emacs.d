;;; -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :init
  (require 'auth-source)
  (let* (
         (auth-info (car (auth-source-search :user "apikey")))
         (host (plist-get auth-info :host))
         (key (plist-get auth-info :secret))
         )
    (setq-default gptel-backend
                  (gptel-make-openai "ChatGPT"
                    :protocol "http"
                    :host host
                    :stream t
                    :key key
                    :models '(gpt-4o))
                  )
    )
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-proxy "http://127.0.0.1:7897")
  (gptel-model 'gpt-4o)
  (gptel-org-branching-context t)
  (gptel-prompt-prefix-alist
   '((markdown-mode . "# ")
     (org-mode . "")
     (text-mode . "# ")))
  (gptel-response-prefix-alist
   '((markdown-mode . "**Response:**\n")
     (org-mode . "*Response:*\n")
     (text-mode . "")))
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (with-eval-after-load 'corfu
    (when (fboundp 'corfu-quit)
      (add-hook 'gptel-pre-response-hook 'corfu-quit)))
  ;; :bind
  ;; ("C-c q" . gptel-send)
  )

;; chatgpt 支持
;; (use-package chatgpt-shell
;;   :straight t
;;   :config
;;   (setq chatgpt-shell-api-url-base "http://ipads.chat.gpt:3006")
;;     ;; 设置代理
;;     (setq chatgpt-shell-additional-curl-options '("-x" "http://127.0.0.1:7897"))
;;
;;   ;; (setq chatgpt-shell-api-url-path "")
;;   (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
;;   (setq chatgpt-shell-prompt-query-response-style #'shell)
;;   (setq chatgpt-shell-prompt-header-describe-code "What does the following code do? Use chinese to answer it")
;;   (setq chatgpt-shell-model-versions '("gpt-4o" "gpt-4o-mini"))
;;   (setq chatgpt-shell-model-version 1)
;;   (setq chatgpt-shell-root-path (expand-file-name "var/" user-emacs-directory))
;;   :bind
;;   ("C-c q" . chatgpt-shell-explain-code)
;;   ;; ("C-c q" . chatgpt-shell)
;;   ;; (global-set-key (kbd "C-d") 'chatgpt-shell)
;;   ("C-d" . chatgpt-shell)
;;   ;; M-n/p 可以查询历史
;;   (:map chatgpt-shell-mode-map
;;         ("M-<return>" . chatgpt-shell-newline)
;;         ("<return>" . chatgpt-shell-submit)
;;         ("C-l" . chatgpt-shell-clear-buffer)))
