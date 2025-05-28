;;; -*- lexical-binding: t -*-

;; (use-package gptel
;;   :straight t
;;   :init
;;   (require 'auth-source)
;;   (let* (
;;          (auth-info (car (auth-source-search :user "apikey")))
;;          (host (plist-get auth-info :host))
;;          (key (plist-get auth-info :secret))
;;          )
;;     (setq-default gptel-backend
;;                   (gptel-make-openai "IPADS"
;;                     :protocol "http"
;;                     :host host
;;                     :stream t
;;                     :key key
;;                     :models '(gpt-4o))
;;                   )
;;     )
;;   :custom
;;   (gptel-default-mode 'org-mode)
;;   (gptel-proxy "http://127.0.0.1:7897")
;;   (gptel-model 'gpt-4o)
;;   (gptel-org-branching-context t)
;;   (gptel-prompt-prefix-alist
;;    '((markdown-mode . "# ")
;;      (org-mode . "")
;;      (text-mode . "# ")))
;;   (gptel-response-prefix-alist
;;    '((markdown-mode . "**Response:**\n")
;;      (org-mode . "*Response:*\n")
;;      (text-mode . "")))
;;   :config
;;   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;;   (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
;;   (with-eval-after-load 'corfu
;;     (when (fboundp 'corfu-quit)
;;       (add-hook 'gptel-pre-response-hook 'corfu-quit)))
;;   ;; And spaces around source code.
;;   (define-advice gptel--replace-source-marker (:after (num-ticks &optional end) add-space)
;;     (unless (= num-ticks 3)
;;       (if end
;;           (unless (or (= (point) (line-end-position))
;;                       (= (char-after) 32))
;;             (insert " "))
;;         (unless (or (= (point) (1+ (line-beginning-position)))
;;                     (= (char-after (- (point) 2)) 32))
;;           (backward-char)
;;           (insert " ")
;;           (forward-char)))))
;;   (defun +gptel--transform-cjk-symbols (beg end)
;;     "Transform CJK symbols from BEG and END to ASCII symbols."
;;     (let ((cjk-transfer-map '(("，" . ", ")
;;                               ("：" . ": ")
;;                               ("。" . ". ")
;;                               ("！" . "! ")
;;                               ("\\*（" . "*(")
;;                               ("）\\*" . ")*")
;;                               ("（" . " (")
;;                               ("）" . ") "))))
;;       (mapc (lambda (recipe)
;;               (replace-regexp-in-region (car recipe)
;;                                         (propertize (cdr recipe) 'gptel 'response) beg end))
;;             cjk-transfer-map)))
;;
;;   (add-hook 'gptel-post-response-functions #'+gptel--transform-cjk-symbols)
;;   (setq gptel--num-messages-to-send 0)
;;   :bind
;;   ("C-c Q" . gptel-menu)
;;   )
;;
;; (use-package gptel-transient
;;   :commands gptel--suffix-send
;;   :config
;;   (defun gptel-send-with-options (&optional arg)
;;     "Send query.  With prefix ARG open gptel's menu instead."
;;     (interactive "P")
;;     (if arg
;;         (call-interactively 'gptel-menu)
;;       (gptel--suffix-send (transient-a
;;                          rgs 'gptel-menu))))
;; :bind
;; ("C-c q" . gptel-send-with-options))

;; chatgpt 支持
;; C-c C-v 切换模型，C-c C-s 切换 prompt
(use-package chatgpt-shell
  :ensure t
  :config
  (require 'auth-source)
  (setq chatgpt-shell-openai-key (auth-source-pick-first-password :host "openai.api.key"))
  (setq chatgpt-shell-deepseek-key (auth-source-pick-first-password :host "deepseek.api.key"))
  (if on-server
      (setq chatgpt-shell-model-version "deepseek-chat")
    (progn
      (setq chatgpt-shell-model-version "gpt-4o")
      (setq chatgpt-shell-proxy "http://127.0.0.1:7897")
      ))
  :custom
  ;; 设置代理
  (chatgpt-shell-api-url-base "http://ipads.chat.gpt:3006")
  (chatgpt-shell-root-path (expand-file-name "var/" user-emacs-directory))
  (chatgpt-shell-model-version "gpt-4o")
  (chatgpt-shell-prompt-header-describe-code "What does the following code do? Use chinese to answer it")
  (chatgpt-shell-prompt-query-response-style #'shell)
  :bind
  ("C-c q" . chatgpt-shell-describe-code)
  ("C-d" . chatgpt-shell)
  ;; M-n/p 可以查询历史
  (:map chatgpt-shell-mode-map
        ("M-<return>" . chatgpt-shell-newline)
        ("<return>" . chatgpt-shell-submit)
        ("C-l" . chatgpt-shell-clear-buffer)))
