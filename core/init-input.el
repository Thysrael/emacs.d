;; -*- lexical-binding: t; -*-

;; ;; 为 rime 的提示框做准备
;; (use-package posframe)
;;
;; ;; emacs rime 输入法前端
;; (use-package rime
;;   :custom
;;   (default-input-method "rime")
;;   :config
;;   (setq rime-user-data-dir (no-littering-expand-etc-file-name "rime/"))
;;   (setq rime-show-candidate 'posframe) ; 设置候选框展示风格
;;   (setq rime-disable-predicates
;;         '(rime-predicate-after-alphabet-char-p ; 在英文字符串之后（必须为以字母开头的英文字符串）
;;           rime-predicate-prog-in-code-p ; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
;;           rime-predicate-space-after-cc-p ; 在中文字符且有空格之后
;;           rime-predicate-current-uppercase-letter-p ; 将要输入的为大写字母时
;;           rime-predicate-tex-math-or-command-p ; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
;;           rime-predicate-punctuation-line-begin-p ; 在行首要输入符号时
;;           ;; rime-predicate-after-ascii-char-p ; 任意英文字符后
;;           ))
;;   :hook
;;   (after-init . toggle-input-method)
;;   (kill-emacs . rime-lib-finalize) ; hack，修正 rime 的关闭问题
;;   )
;;
;; 在 emacs 状态下不使用搜狗输入法，进而使用 emacs 内的 rime 输入法
;; (defvar input-toggle nil "Toggle variable for input method, when nil means English, true means Chinese.")
;;
;; (defun fcitx2en ()
;;   "Change to the English input."
;;   (let ((input-status (process-lines "fcitx-remote")))
;;     (when (= (string-to-number (car input-status)) 1) ; input status == 1 时表示搜狗输入法
;;       (setq input-toggle nil)
;;       (start-process "fcitx-remote-process" nil "fcitx-remote" "-o")
;;       (message ""))))
;;
;; (defun fcitx2zh ()
;;   "Change to the Chinese input."
;;   (let ((input-status (process-lines "fcitx-remote")))
;;     (unless (and (not (= (string-to-number (car input-status)) 1)) input-toggle)
;;       (start-process "fcitx-remote-process" nil "fcitx-remote" "-c")
;;       (setq input-toggle t))))
;;
;; (setq focus-in-hook 'fcitx2en)
;; (setq focus-out-hook 'fcitx2zh)
;; (add-hook 'kill-emacs-hook 'fcitx2zh)


;; 这个方案比 emacs rime 提供的中英文 inline 方便
;; (use-package sis
;;   :hook
;;   (((text-mode prog-mode) . sis-context-mode)
;;    ((text-mode prog-mode) . sis-inline-mode))
;;   :config
;;   (sis-ism-lazyman-config "1" "2" 'fcitx)
;;   ;; enable the /cursor color/ mode
;;   (sis-global-cursor-color-mode t)
;;   ;; enable the /respect/ mode
;;   (sis-global-respect-mode t)
;;   ;; enable the /context/ mode for all buffers
;;   (sis-global-context-mode t)
;;   ;; enable the /inline english/ mode for all buffers
;;   (sis-global-inline-mode t)
;;   :custom
;;   (sis-other-cursor-color "#c3e88d")
;;   )

(use-package rime
  :straight (rime :type git
                  :host github
                  :repo "DogLooksGood/emacs-rime"
                  :files ("*.el" "Makefile" "lib.c"))
  :custom-face
  (rime-default-face ((t (:inherit hl-line
                                   :background unspecified :foreground unspecified))))
  (rime-preedit-face ((t (:inherit hl-line
                                   :background unspecified
                                   :inverse-video unspecified :underline t))))
  :init
  (require 'rime)
  :config
  (setq rime-user-data-dir (no-littering-expand-etc-file-name "rime/"))
  (setq rime-disable-predicates
        '(rime-predicate-after-alphabet-char-p ; 在英文字符串之后（必须为以字母开头的英文字符串）
          rime-predicate-prog-in-code-p ; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
          rime-predicate-space-after-cc-p ; 在中文字符且有空格之后
          rime-predicate-current-uppercase-letter-p ; 将要输入的为大写字母时
          ;; rime-predicate-tex-math-or-command-p ; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
          rime-predicate-punctuation-line-begin-p ; 在行首要输入符号时
          rime-predicate-after-ascii-char-p ; 任意英文字符后
          ))

  (setq rime-show-candidate 'posframe
        rime-show-preedit 'inline
        rime-posframe-properties '(:internal-border-width 7))

  (defadvice! +rime-do-finalize-after-loading-module (&rest _)
    :after #'rime--load-dynamic-module
    (add-hook! kill-emacs-hook #'rime-lib-finalize))
  :bind
  ("C-M-\\" . rime-force-enable)
  )

;; (use-package key-echo
;;   :straight nil
;;   :after rime
;;   :init
;;   (add-to-list 'load-path "~/.emacs.d/site-lisp/key-echo/")
;;   (require 'key-echo)
;;   (key-echo-enable)
;;   (defun key-echo-shift-to-switch-input-method (key)
;;     (interactive)
;;     (when (string-equal key "Key.shift")
;;       (toggle-input-method)
;;       ))
;;   (setq key-echo-single-key-trigger-func 'key-echo-shift-to-switch-input-method)
;;   ;; WORKAROUND: I don't know why I have to add this
;;   (key-echo-restart-process)
;;   )

;; [sis] automatically switch input source
(use-package sis
  :hook
  (((text-mode prog-mode) . sis-context-mode)
   ((text-mode prog-mode) . sis-inline-mode))
  :custom
  (sis-other-cursor-color "#c3e88d")
  :config
  (add-hook! (+theme-changed-hook server-after-make-frame-hook) :call-immediately
    (defun +sis-set-other-cursor-color ()
      (setq sis-other-cursor-color (face-foreground 'error nil t))))
  ;; Use emacs-rime as default
  (sis-ism-lazyman-config nil "rime" 'native)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t))
