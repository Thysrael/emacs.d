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
(defvar input-toggle nil "Toggle variable for input method, when nil means English, true means Chinese.")

(defun fcitx2en ()
  "Change to the English input."
  (let ((input-status (process-lines "fcitx-remote")))
    (when (= (string-to-number (car input-status)) 1) ; input status == 1 时表示搜狗输入法
      (setq input-toggle nil)
      (start-process "fcitx-remote-process" nil "fcitx-remote" "-o")
      (message ""))))

(defun fcitx2zh ()
  "Change to the Chinese input."
  (let ((input-status (process-lines "fcitx-remote")))
    (unless (and (not (= (string-to-number (car input-status)) 1)) input-toggle)
      (start-process "fcitx-remote-process" nil "fcitx-remote" "-c")
      (setq input-toggle t))))

(setq focus-in-hook 'fcitx2en)
(setq focus-out-hook 'fcitx2zh)
(add-hook 'kill-emacs-hook 'fcitx2zh)


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
          rime-predicate-tex-math-or-command-p ; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
          rime-predicate-punctuation-line-begin-p ; 在行首要输入符号时
          ;; rime-predicate-after-ascii-char-p ; 任意英文字符后
          ))

  (setq rime-show-candidate 'posframe
        rime-show-preedit 'inline
        rime-posframe-properties '(:internal-border-width 7))

  (defadvice! +rime-do-finalize-after-loading-module (&rest _)
    :after #'rime--load-dynamic-module
    (add-hook! kill-emacs-hook #'rime-lib-finalize))
  )


;; [sis] automatically switch input source
(use-package sis
  :hook
  (((text-mode prog-mode) . sis-context-mode)
   ((text-mode prog-mode) . sis-inline-mode))
  :custom
  (sis-other-cursor-color "#c3e88d")
  :config
  ;; Use emacs-rime as default
  (sis-ism-lazyman-config nil "rime" 'native)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)

  (defun +sis-remove-head-space-after-cc-punc (_)
    (when (or (memq (char-before) '(?， ?。 ?？ ?！ ?； ?： ?（ ?【 ?「 ?“))
                                                                (bolp))
                                        (delete-char 1)))
              (setq sis-inline-tighten-head-rule #'+sis-remove-head-space-after-cc-punc)

              (defun +sis-remove-tail-space-before-cc-punc (_)
                (when (eq (char-before) ? )
                  (backward-delete-char 1)
                  (when (and (eq (char-before) ? )
                             (memq (char-after) '(?， ?。 ?？ ?！ ?； ?： ?（ ?【 ?「 ?“)))
                                                      (backward-delete-char 1))))
                        (setq sis-inline-tighten-tail-rule #'+sis-remove-tail-space-before-cc-punc)

                        ;; Context mode
                        (add-hook 'meow-insert-exit-hook #'sis-set-english)
                        (add-to-list 'sis-context-hooks 'meow-insert-enter-hook)

                        ;; Ignore some mode with context mode
                        (defadvice! +sis-context-guess-ignore-modes (fn &rest args)
                          :around #'sis--context-guess
                          (if (derived-mode-p 'pdf-view-mode)
                              'english
                            (apply fn args)))

                        (defun +sis-context-switching-other (back-detect fore-detect)
                          (when (and meow-insert-mode
                                     (or (and (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
                                              (sis--context-other-p back-detect fore-detect))
                                         (and (derived-mode-p 'telega-chat-mode)
                                              (or (and (= (point) telega-chatbuf--input-marker) ; beginning of input
                                                       (eolp))
                                                  (sis--context-other-p back-detect fore-detect)))))
                            'other))

                        (add-to-list 'sis-context-detectors #'+sis-context-switching-other)

                        ;; Inline-mode
                        (defvar-local +sis-inline-english-last-space-pos nil
                          "The last space position in inline mode.")

                        (add-hook! sis-inline-english-deactivated-hook
                          (defun +sis-line-set-last-space-pos ()
                            (when (eq (char-before) ?\s)
                              (setq +sis-inline-english-last-space-pos (point)))))

                        (add-hook! sis-inline-mode-hook
                          (defun +sis-inline-add-post-self-insert-hook ()
                            (add-hook! post-self-insert-hook :local
                              (defun +sis-inline-remove-redundant-space ()
                                (when (and (eq +sis-inline-english-last-space-pos (1- (point)))
                                           (looking-back " [，。？！；：（【「“]"))
                                  (save-excursion
                                    (backward-char 2)
                                    (delete-char 1)
                                    (setq-local +sis-inline-english-last-space-pos nil)
                                    ))
                                ))
                            ))
                        )
