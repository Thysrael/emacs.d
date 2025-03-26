;;; -*- lexical-binding: t -*-

;;; 高亮设置
;; 高亮当前行
(use-package hl-line
  :straight t
  :hook ((prog-mode text-mode
                    yaml-mode conf-mode
                    special-mode org-agenda-mode dired-mode) . hl-line-mode))

;; 显示配对扩号
(use-package paren
  :straight t
  :custom-face (show-paren-match ((t (:underline t))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t)
  )

;; 彩虹扩号
(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)
  )

;; 高亮 TODO, BUG 等关键词
(use-package hl-todo
  :straight t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :inverse-video t))))
  :hook
  ((prog-mode conf-mode yaml-mode LaTeX-mode) . hl-todo-mode)
  :config
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")

  (defun +hl-todo-add-keywords (keys color)
    (dolist (keyword keys)
      (if-let ((item (assoc keyword hl-todo-keyword-faces)))
          (setf (cdr item) color)
        (push `(,keyword . ,color) hl-todo-keyword-faces))))

  ;; HACK: `hl-todo' won't update face when changing theme, so we must add a hook for it
  (defun +hl-update-keyword-faces (&rest _)
    (+hl-todo-add-keywords '("BUG" "DEFECT" "ISSUE") (face-foreground 'error))
    (+hl-todo-add-keywords '("WORKAROUND" "HACK" "TRICK") (face-foreground 'warning)))
  (+hl-update-keyword-faces)
  (advice-add #'enable-theme :after #'+hl-update-keyword-faces)
  )

;; 缩进虚线
(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2)

  ;; HACK: `indent-bars' calculates its faces from the current theme,
  ;; but is unable to do so properly in terminal Emacs
  (defun +indent-bars-auto-set-faces (&rest _)
    (when indent-bars-mode
      (indent-bars-reset)))
  (advice-add #'enable-theme :after #'+indent-bars-auto-set-faces)
  )

;;; 重构设置
;; 将需要高亮的符号进行高亮，当光标在 overlay 区域时，会触发新的快捷键用于操作符号
(use-package symbol-overlay
  :straight t
  :bind
  ("<f7>" . symbol-overlay-put)
  ("<f8>" . symbol-overlay-remove-all)
  (:map symbol-overlay-map
        ("N" . symbol-overlay-jump-next)
        ("P" . symbol-overlay-jump-prev)
        ("n" . nil)
        ("p" . nil)
        ("e" . nil)
        ("d" . nil)
        ("s" . nil))
  :hook
  (((prog-mode yaml-mode) . symbol-overlay-mode))
  )

;; 撤销历史, 使用 a / e / f / b 移动
(use-package vundo
  :straight t
  :config
  (setq vundo-compact-display t)
  :bind
  ("C-c u" . vundo)
  )

;; 智能注释
(defun +smart-comment (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "C-/") '+smart-comment)
(setq comment-empty-lines t) ; comment over empty lines

;; 自动格式化
(defun +smart-format ()
  "Indent the active region if selected, otherwise format the buffer using eglot if available."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode) (eglot-managed-p)) ; 判断 eglot
      (if (use-region-p)
          (eglot-format (region-beginning) (region-end))
        (eglot-format-buffer))
    (if (use-region-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (point-min) (point-max))))
  (message "formatting done."))
(global-set-key (kbd "C-c f") '+smart-format)

;;; 结构化编辑
;; 括号平衡
(use-package elec-pair
  :straight t
  :hook
  ((prog-mode text-mode) . electric-pair-mode)
  :custom
  (electric-pair-pairs '(
                         (?\" . ?\")
                         (?\‘ . ?\’)
                         (?\“  . ?\”)
                         ;; WORKAROUND: 引入两个反扩号配对来解决中文引号问题
                         (?\’ . ?\‘)
                         (?\” . ?\“)
                         ))
  :config
  ;; 修正中文引号问题：修正左右对调的补全
  (define-advice electric-pair--insert (:around (orig-fn c) fix-curved-quotes)
    (let* ((qpair (rassoc c electric-pair-pairs))
           (reverse-p (and qpair (> (car qpair) (cdr qpair)))))
      (if reverse-p
          (run-with-timer 0 nil
                          `(lambda ()
                             (backward-char 1)
                             (insert (char-to-string ,c))))
        (funcall orig-fn c))))
  ;; 禁用 org-mode 的左尖括号
  ;; :config
  ;; (setq electric-pair-inhibit-predicate
  ;;       `(lambda (c)
  ;;          (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))
  ;;
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (setq-local electric-pair-inhibit-predicate
  ;;   		              `(lambda (c)
  ;;   		                 (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))))
  )

;; 空格处理
(use-package ws-butler
  :straight t
  :hook ((prog-mode markdown-mode) . ws-butler-mode)) ; Remove trailing whitespace with lines touched
(setq backward-delete-char-untabify-method 'hungry) ; 一次删除多个空格

;; 处理类似驼峰命名法的情况，此时会将一个一个驼峰视为一个单词
(use-package subword
  :straight nil
  :hook (((prog-mode minibuffer-setup) . subword-mode)))

(use-package hideshow
  :hook ((prog-mode conf-mode yaml-mode TeX-mode nxml-mode) . hs-minor-mode)
  :bind
  ("C-o" . hs-toggle-hiding)
  ("C-M-o" . hs-toggle-all)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun +hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (let ((lines (number-to-string (count-lines (overlay-start ov) (overlay-end ov)))))
                     (concat " "
                             (propertize (concat " ... L" lines " ") 'face '(:inherit shadow :height 0.8 :box t))
                             " "))
                   )))
  (setq hs-set-up-overlay #'+hs-display-code-line-counts)

  ;; hide-show by indentation
  (defun +fold--hideshow-empty-line-p (_)
    (string= "" (string-trim (thing-at-point 'line 'no-props))))

  (defun +fold--hideshow-geq-or-empty-p (base-indent)
    (or (+fold--hideshow-empty-line-p base-indent)
        (>= (current-indentation) base-indent)))

  (defun +fold--hideshow-g-or-empty-p (base-indent)
    (or (+fold--hideshow-empty-line-p base-indent)
        (> (current-indentation) base-indent)))

  (defun +fold--hideshow-seek (start direction before skip predicate base-indent)
    "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
    (save-excursion
      (goto-char start)
      (goto-char (point-at-bol))
      (let ((bnd (if (> 0 direction)
                     (point-min)
                   (point-max)))
            (pt (point)))
        (when skip (forward-line direction))
        (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
                 do (progn
                      (when before (setq pt (point-at-bol)))
                      (forward-line direction)
                      (unless before (setq pt (point-at-bol)))))
        pt)))

  (defun +fold-hideshow-indent-range (&optional point)
    "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
    (save-excursion
      (when point
        (goto-char point))
      (let ((base-indent (current-indentation))
            (begin (point))
            (end (point)))
        (setq begin (+fold--hideshow-seek begin -1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
              begin (+fold--hideshow-seek begin 1 nil nil #'+fold--hideshow-g-or-empty-p base-indent)
              end   (+fold--hideshow-seek end 1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
              end   (+fold--hideshow-seek end -1 nil nil #'+fold--hideshow-empty-line-p base-indent))
        (list begin end base-indent))))

  (defun +fold-hideshow-forward-block-by-indent-fn (_arg)
    (let ((start (current-indentation)))
      (forward-line)
      (unless (= start (current-indentation))
        (let ((range (+fold-hideshow-indent-range)))
          (goto-char (cadr range))
          (end-of-line)))))

  ;; support for special modes
  (add-to-list 'hs-special-modes-alist
               '(LaTeX-mode
                 ;; LaTeX-find-matching-end needs to be inside the env
                 ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
                 "\\\\end{[a-zA-Z*]+}"
                 "%"
                 (lambda (_arg)
                   ;; Don't fold whole document, that's useless
                   (unless (save-excursion
                             (search-backward "\\begin{document}"
                                              (line-beginning-position) t))
                     (LaTeX-find-matching-end)))
                 nil)
               )
  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "do" "{" "[" "if" "while")) ; Block start
                 ,(rx (or "}" "]" "end"))                       ; Block end
                 ,(rx (or "#" "=begin"))                        ; Comment start
                 ruby-forward-sexp nil))
  (add-to-list 'hs-special-modes-alist
               '(yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                           ""
                           "#"
                           +fold-hideshow-forward-block-by-indent-fn nil))
  (add-to-list 'hs-special-modes-alist
               '(matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                             "end"
                             nil (lambda (_arg) (matlab-forward-sexp))))
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode "<!--\\|<[^/>]*[^/]>"
                           "-->\\|</[^/>]*[^/]>"
                           "<!--" sgml-skip-tag-forward nil))
  )

;; 可以快速选择区域
(use-package expand-region
  :straight t
  :bind
  ("C-l" . er/expand-region)
  ("C-M-l" . er/contract-region))

;; 用于强化删除功能，可以平衡删除
(use-package puni
  :straight t
  :hook
  ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :bind
  (:map puni-mode-map
        ("C-w" . nil)
        ("C-d" . nil)
        ("<backspace>" . nil)
        ("<DEL>" . nil)))

;; 快速编辑成对出现的标点
(use-package embrace
  :straight t
  :bind
  ("C-." . embrace-commander)
  :hook
  (org-mode . embrace-org-mode-hook)
  )

;;; misc
;; [sudo-edit] edit file with su permissions
(use-package sudo-edit
  :straight t
  :config
  (sudo-edit-indicator-mode t)
  )

;; [wgrep] Edit a grep buffer and apply changes to the file buffer
(use-package wgrep
  :straight t
  :config
  (setq wgrep-auto-save-buffer t)
  )
