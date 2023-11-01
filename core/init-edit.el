;;; -*- lexical-binding: t -*-

;;; 高亮设置
;; 高亮当前行
(use-package hl-line
  :hook ((prog-mode text-mode
                    yaml-mode conf-mode
                    special-mode org-agenda-mode dired-mode) . hl-line-mode))

;; 显示配对扩号
(use-package paren
  :custom-face (show-paren-match ((t (:underline t))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t)
  )

;; 显示错误的空白字符，并没有很好的效果
;; (use-package whitespace
;;   :hook ((prog-mode conf-mode yaml-mode) . whitespace-mode)
;;   :init
;;   :config
;;   (setq
;;    ;; only show bad whitespace
;;    whitespace-style '(face trailing empty indentation space-before-tab space-after-tab)))

;; 彩虹扩号
(use-package rainbow-delimiters
  :hook ((prog-mode conf-mode yaml-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)
  )


;; 加粗父扩号
(use-package highlight-parentheses
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("SpringGreen3" "IndianRed1" "IndianRed3" "IndianRed4")
        highlight-parentheses-attributes '((:underline t :weight bold)
                                           (:underline t :weight bold)
                                           (:underline t :weight bold))
        highlight-parentheses-delay 0.2))


;; 展示颜色，用于 CSS 或者 HTML
(use-package rainbow-mode
  :functions (rainbow-turn-off rainbow-colorize-match rainbow-x-color-luminance)
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode)
  )


;; [hl-todo] Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :inverse-video t))))
  :hook
  ((prog-mode conf-mode yaml-mode) . hl-todo-mode)
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

;; 缩进高亮
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

;; 当进行复制、粘贴和剪切等行为的时候，会高亮这个区域
;; (use-package goggles
;;   :hook
;;   ((prog-mode conf-mode yaml-mode text-mode) . goggles-mode)
;;   :config
;;   (setq-default goggles-pulse nil)
;;   )

;; 将需要高亮的符号进行高亮
(use-package symbol-overlay
  :bind
  ("<f6>" . symbol-overlay-rename)
  ("<f7>" . symbol-overlay-put)
  ("<f8>" . symbol-overlay-remove-all)
  :hook
  (((prog-mode yaml-mode) . symbol-overlay-mode))
  )

;; 注释
(defun smart-comment (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "C-/") 'smart-comment)

;; comment over empty lines
(setq comment-empty-lines t)

;; 搜索替换
(use-package color-rg
  :straight (color-rg :type git :host github :repo "manateelazycat/color-rg")
  :custom
  (color-rg-search-no-ignore-file nil) ; 进行项目搜索时忽略文件
  :bind
  ("C-r" . my-color-rg-search-input-in-buffer)
  ("C-c r" . my-color-rg-search-input-in-project)
  :config
  (defun my-color-rg-search-input-in-buffer ()
    "Color-rg in the current file or perform query-replace based on the buffer's association with a file."
    (interactive)
    (if (buffer-file-name)
        (color-rg-search-input-in-current-file)
      (call-interactively 'query-replace)))

  (defun my-color-rg-search-input-in-project ()
    "Color-rg in the project."
    (interactive)
    (if (buffer-file-name)
        (color-rg-search-input-in-project)
      (message "Not in a project"))))

;; Do not add the duplicates that the same as the last one to kill-ring
(setq kill-do-not-save-duplicates t)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; [autorevert] TODO: Add hooks as what doom has done?
(use-package autorevert
  :hook
  (after-init . global-auto-revert-mode)
  :config
  ; Only prompts for confirmation when buffer is unsaved.
  (setq revert-without-query (list "."))
  )

;; [goto-addr] Click to open URL
(use-package goto-addr
  :straight nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; [ws-butler] Remove trailing whitespace with lines touched
(use-package ws-butler
  :hook ((prog-mode markdown-mode) . ws-butler-mode))

;; [ediff] Diff & patch
(use-package ediff
  :straight nil
  :hook
  ((ediff-before-setup . +ediff-save-window-config)
         ((ediff-quit ediff-suspend) . +ediff-restore-window-config))
  :functions (outline-show-all)
  :config

  ;; unfold outlines when using ediff
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'outline-show-all))

  ;; Restore window config after quitting ediff
  (defvar +ediff-saved-window-config nil)
  (defun +ediff-save-window-config ()
    (setq +ediff-saved-window-config (current-window-configuration)))
  (defun +ediff-restore-window-config ()
    (when (window-configuration-p +ediff-saved-window-config)
      (set-window-configuration +ediff-saved-window-config)))

  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally
        ;; turn off whitespace checking
        ediff-diff-options "-w")
  )

;; [elec-pair] Automatic parenthesis pairing
(use-package elec-pair
  :hook
  ((prog-mode) . electric-pair-mode)
  :config
  (add-hook 'org-mode-hook
          (lambda ()
            (setq-local electric-pair-mode nil)))
  )

;; 一口气删掉多个空白格
(setq backward-delete-char-untabify-method 'hungry)

;; 处理类似驼峰命名法的情况，此时会将一个一个驼峰视为一个单词
(use-package subword
  :straight nil
  :hook (((prog-mode minibuffer-setup) . subword-mode)))

;; 代码折叠
(use-package hideshow
  :straight nil
  :hook ((prog-mode conf-mode yaml-mode) . hs-minor-mode)
  :bind
  ("C-o" . hs-toggle-hiding)
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
  (setq hs-special-modes-alist
        (append
         '((yaml-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                      ""
                      "#"
                      +fold-hideshow-forward-block-by-indent-fn nil)
           (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
                      "end\\|[]}]"
                      "#\\|=begin"
                      ruby-forward-sexp)
           (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
                        "end"
                        nil (lambda (_arg) (matlab-forward-sexp)))
           (nxml-mode "<!--\\|<[^/>]*[^/]>"
                      "-->\\|</[^/>]*[^/]>"
                      "<!--" sgml-skip-tag-forward nil)
           (latex-mode
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
            nil))
         hs-special-modes-alist
         '((t))))
  )

;; 可以快速选择区域
(use-package expand-region
  :bind ("C-l" . er/expand-region))

;; [vundo] Undo tree, 使用 f / b 移动
(use-package vundo
  :config
  (setq vundo-compact-display t)
  :bind
  ("C-c u" . vundo)
  )

;; [sudo-edit] edit file with su permissions
(use-package sudo-edit
  :config
  (sudo-edit-indicator-mode t)
  )

;; [wgrep] Edit a grep buffer and apply changes to the file buffer
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  )

;; 用于强化删除功能，可以平衡删除
(use-package puni
  :hook
  ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :bind
  ("DEL" . +puni-hungry-delete)
  :config
  (defun +puni-hungry-delete ()
    (interactive)
    (if (looking-back "^[[:blank:]]+")
        (let* ((puni-mode nil)
               (original-func (key-binding (kbd "DEL"))))
          ;; original-func is what `DEL' would be if puni-mode were disabled
          (if (eq original-func 'delete-backward-char)
              (backward-delete-char-untabify 1)
            (call-interactively original-func)))
      (puni-backward-delete-char)))
  (define-key puni-mode-map (kbd "C-k") nil)
  (define-key puni-mode-map (kbd "C-d") 'puni-kill-line)
  (define-key puni-mode-map (kbd "C-w") nil)
  )

;; [ispell] spell checker
(use-package ispell
  :straight nil
  :hook ((org-mode . org-skip-region-alist)
         (markdown-mode . markdown-skip-region-alist))
  :config
  ;; Don't spellcheck org blocks
  (defun org-skip-region-alist ()
    (make-local-variable 'ispell-skip-region-alist)
    (dolist (pair '((org-property-drawer-re)
                    ("~" "~") ("=" "=")
                    ("^#\\+BEGIN_SRC" "^#\\+END_SRC")
                    ("\\\\(" "\\\\)") ("\\[" "\\]")
                    ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))

  (defun markdown-skip-region-alist ()
    (make-local-variable 'ispell-skip-region-alist)
    (dolist (pair '(("`" "`")
                    ("^```" "^```")
                    ("{{" "}}")
                    ("\\\\(" "\\\\)") ("\\[" "\\]")
                    ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))

  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--run-together")
        ispell-dictionary "en_US")

  (setq ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir")
        ispell-personal-dictionary (expand-file-name "ispell/.pws" user-emacs-directory))
  )

;; [embrace] Add/change/delete pairs of symbol
(use-package embrace
  :bind
  ("C-." . embrace-commander)
  :hook
  (org-mode . embrace-org-mode-hook)
  )


;; [imenu] Jump to function definitions
(use-package imenu
  :straight nil
  :hook ((prog-mode conf-mode yaml-mode markdown-mode org-mode) . imenu-add-menubar-index)
  :config
  (setq imenu-auto-rescan t)
  )

;; 增强注释
(defun +smart-comment (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "C-/") '+smart-comment)

;; comment over empty lines
(setq comment-empty-lines t)

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

(defhydra Cx-r (
                :hint nil ; 只显示注释字符串，不显示绑定信息
                :color blue ; 执行完一次后就退出
                :foreign-keys run ; 如果不在 hydra 按键内，则执行，并不退出 hydra
                )
  "
        Bookmark^^        Register^^        Rectangle^^
  --------------------------------------------------------
        [_l_] List        [_v_] List        [_M_] Mark
        [_m_] Mark        [_SPC_] Point     [_N_] Number
        [_b_] Jump        [_s_] Text        [_t_] String
        ^ ^               [_r_] Rectangle   [_o_] Space
        ^ ^               [_w_] Window      [_c_] Clear
        ^ ^               [_K_] Kmacro      [_k_] Kill
        [_q_] Quit        ^ ^               [_y_] Yank
  "
  ("m" bookmark-set-no-overwrite)
  ("b" bookmark-jump)
  ("l" bookmark-bmenu-list)

  ("v" consult-register)
  ("SPC" point-to-register)
  ("s" copy-to-register)
  ("r" copy-rectangle-to-register)
  ("w" window-configuration-to-register)
  ("K" kmacro-to-register)

  ("M" rectangle-mark-mode :color red) ; red 执行完后不退出
  ("N" rectangle-number-lines :color red)
  ("t" string-rectangle :color red)
  ("o" open-rectangle :color red)
  ("c" clear-rectangle :color red)
  ("k" kill-rectangle :color red)
  ("y" yank-rectangle :color red)

  ("q" nil))
(global-set-key (kbd "C-x r") 'Cx-r/body)

;; (use-package multiple-cursors
;;   :after hydra
;;   :bind
;;   (("C-c m" . hydra-multiple-cursors/body)
;;    ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
;;   :config
;;   (defhydra hydra-multiple-cursors (:hint nil)
;;     "
;; Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
;; ------------------------------------------------------------------
;;  [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
;;  [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
;;  [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
;;  [_|_] Align with input CHAR       [Click] Cursor at point"
;; 		  ("l" mc/edit-lines :exit t)
;; 		  ("a" mc/mark-all-like-this :exit t)
;; 		  ("n" mc/mark-next-like-this)
;; 		  ("N" mc/skip-to-next-like-this)
;; 		  ("M-n" mc/unmark-next-like-this)
;; 		  ("p" mc/mark-previous-like-this)
;; 		  ("P" mc/skip-to-previous-like-this)
;; 		  ("M-p" mc/unmark-previous-like-this)
;; 		  ("|" mc/vertical-align)
;; 		  ("s" mc/mark-all-in-region-regexp :exit t)
;; 		  ("0" mc/insert-numbers :exit t)
;; 		  ("A" mc/insert-letters :exit t)
;; 		  ("<mouse-1>" mc/add-cursor-on-click)
;; 		  ;; Help with click recognition in this hydra
;; 		  ("<down-mouse-1>" ignore)
;; 		  ("<drag-mouse-1>" ignore)
;; 		  ("q" nil)))
