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
;; ;; 在 emacs 状态下不使用搜狗输入法，进而使用 emacs 内的 rime 输入法
;; (defvar input-toggle nil "Toggle variable for input method, when nil means English, true means Chinese.")
;;
;; (defun fcitx2en ()
;;   "Change to the English input."
;;   (let ((input-status (shell-command-to-string "fcitx-remote")))
;;     (when (= (string-to-number input-status) 1) ; input status == 1 时表示搜狗输入法
;;       (setq input-toggle nil)
;;       (shell-command "fcitx-remote -o")
;;       (message ""))))
;;
;; (defun fcitx2zh ()
;;   "Change to the Chinese input."
;;   (let ((input-status (shell-command-to-string "fcitx-remote")))
;;     (unless (and (not (= (string-to-number input-status) 1)) input-toggle)
;;       (shell-command "fcitx-remote -c")
;;       (setq input-toggle t))))
;;
;; (setq focus-in-hook 'fcitx2en)
;; (setq focus-out-hook 'fcitx2zh)
;; (add-hook 'kill-emacs-hook 'fcitx2zh)


;; 这个方案比 emacs rime 提供的中英文 inline 方便
(use-package sis
  :hook
  (((text-mode prog-mode) . sis-context-mode)
   ((text-mode prog-mode) . sis-inline-mode))
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  :custom
  (sis-other-cursor-color "#c3e88d")
  )
