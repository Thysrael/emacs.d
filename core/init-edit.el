;;; -*- lexical-binding: t -*-

;;; Highlighting
;; Highlight the current line.
(use-package hl-line
  :ensure nil
  :hook ((prog-mode text-mode
                    conf-mode
                    special-mode org-agenda-mode dired-mode) . hl-line-mode))

;; Highlight matching delimiters.
(use-package paren
  :ensure nil
  :custom-face (show-paren-match ((t (:underline t))))
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-context-when-offscreen t)
  )

;; Color nested delimiters.
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode conf-mode yaml-ts-mode) . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 5)
  )

;; Highlight TODO, BUG, and similar keywords.
(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :inverse-video t))))
  :hook
  ((prog-mode conf-mode yaml-ts-mode LaTeX-mode) . hl-todo-mode)
  :config
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":")

  (defun thy/hl-todo-add-keywords (keys color)
    "Add or update hl-todo KEYS using COLOR."
    (dolist (keyword keys)
      (if-let* ((item (assoc keyword hl-todo-keyword-faces)))
          (setf (cdr item) color)
        (push `(,keyword . ,color) hl-todo-keyword-faces))))

  ;; HACK: `hl-todo' won't update face when changing theme, so we must add a hook for it
  (defun thy/hl-update-keyword-faces (&rest _)
    "Refresh custom hl-todo faces for the current theme."
    (thy/hl-todo-add-keywords '("BUG" "DEFECT" "ISSUE") (face-foreground 'error))
    (thy/hl-todo-add-keywords '("WORKAROUND" "HACK" "TRICK") (face-foreground 'warning)))
  (thy/hl-update-keyword-faces)
  (advice-add #'enable-theme :after #'thy/hl-update-keyword-faces)
  )

;; Display indentation guides.
(use-package indent-bars
  ;; :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :ensure t
  :hook (prog-mode . indent-bars-mode)
  :config
  (setq indent-bars-display-on-blank-lines nil
        indent-bars-width-frac 0.2)

  ;; HACK: `indent-bars' calculates its faces from the current theme,
  ;; but is unable to do so properly in terminal Emacs
  (defun thy/indent-bars-auto-set-faces (&rest _)
    "Refresh indent-bars faces after a theme change."
    (when indent-bars-mode
      (indent-bars-reset)))
  (advice-add #'enable-theme :after #'thy/indent-bars-auto-set-faces)
  )

;;; Refactoring
;; Highlight symbols and expose symbol operations at highlighted occurrences.
(use-package symbol-overlay
  :ensure t
  :bind
  ("<f7>" . symbol-overlay-put)
  ("<f8>" . symbol-overlay-remove-all)
  (:map symbol-overlay-map
        ("n" . symbol-overlay-jump-next)
        ("N" . symbol-overlay-jump-prev)
        ("r" . symbol-overlay-rename)
        ("R" . symbol-overlay-query-replace)
        ("/" . symbol-overlay-isearch-literally)
        ("q" . symbol-overlay-remove-all)
        ("p" . nil)
        ("e" . nil)
        ("d" . nil)
        ("s" . nil))
  :hook
  (((prog-mode yaml-ts-mode) . symbol-overlay-mode))
  )

;; Visualize undo history; navigate with a, e, f, and b.
(use-package vundo
  :ensure t
  :config
  (setq vundo-compact-display t)
  :bind
  ("C-c u" . vundo)
  )

;; Context-aware commenting.
(use-package newcomment
  :ensure nil
  :preface
  (defun thy/smart-comment (&optional arg)
    "Comment the current line or invoke `comment-dwim' with ARG."
    (interactive "*P")
    (comment-normalize-vars)
    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (comment-dwim arg)))
  :bind ("C-/" . thy/smart-comment)
  :custom
  (comment-empty-lines t))

;; Format code automatically.
(use-package emacs
  :ensure nil
  :preface
  (defun thy/format-region-or-buffer (&optional beg end)
    "Format BEG to END, or the entire buffer when either is nil."
    (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
        (eglot-format beg end)
      (indent-region (or beg (point-min)) (or end (point-max))))
    (when (bound-and-true-p ws-butler-mode)
      (if (and beg end)
          (ws-butler-clean-region beg end)
        (ws-butler-before-save))))

  (defun thy/smart-format ()
    "Format the region or buffer, then trim whitespace on modified lines."
    (interactive)
    (let* ((regionp (use-region-p))
           (beg (and regionp (copy-marker (region-beginning))))
           (end (and regionp (copy-marker (region-end) t))))
      (unwind-protect
          (progn
            (thy/format-region-or-buffer beg end)
            (message "formatting done."))
        (when beg
          (set-marker beg nil))
        (when end
          (set-marker end nil)))))
  :bind ("C-c f" . thy/smart-format))

;;; Structured editing
;; Keep delimiters balanced.
(use-package elec-pair
  :ensure nil
  :hook
  ((prog-mode text-mode) . electric-pair-mode)
  ;; Do not treat the opening angle bracket as a pair in Org buffers.
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

;; Whitespace handling.
(use-package ws-butler
  :ensure t
  :hook ((prog-mode markdown-mode markdown-ts-mode) . ws-butler-mode)) ; Remove trailing whitespace with lines touched

(use-package simple
  :ensure nil
  :custom
  (backward-delete-char-untabify-method 'hungry)) ; 一次删除多个空格

;; Treat each component of a camelCase identifier as a word.
(use-package subword
  :ensure nil
  :hook (((prog-mode minibuffer-setup) . subword-mode)))

(use-package hideshow
  :ensure nil
  :hook ((prog-mode conf-mode yaml-ts-mode TeX-mode nxml-mode) . hs-minor-mode)
  :bind
  ("C-M-o" . thy/hs-toggle-all)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun thy/hs-cycle (&optional level)
    "Cycle visibility at point or hide LEVEL levels of blocks."
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('thy/hs-cycle
             (hs-hide-level 1)
             (setq this-command 'thy/hs-cycle-children))
            ('thy/hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'thy/hs-cycle-subtree))
            ('thy/hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'thy/hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun thy/hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('thy/hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun thy/hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (let ((lines (number-to-string (count-lines (overlay-start ov) (overlay-end ov)))))
                     (concat " "
                             (propertize (concat " ... L" lines " ") 'face '(:inherit shadow :height 0.8 :box t))
                             " "))
                   )))
  (setq hs-set-up-overlay #'thy/hs-display-code-line-counts)

  ;; hide-show by indentation
  (defun thy/fold--hideshow-empty-line-p (_)
    "Return non-nil when the current line is empty."
    (string= "" (string-trim (thing-at-point 'line 'no-props))))

  (defun thy/fold--hideshow-geq-or-empty-p (base-indent)
    "Return non-nil for empty lines or indentation at least BASE-INDENT."
    (or (thy/fold--hideshow-empty-line-p base-indent)
        (>= (current-indentation) base-indent)))

  (defun thy/fold--hideshow-g-or-empty-p (base-indent)
    "Return non-nil for empty lines or indentation greater than BASE-INDENT."
    (or (thy/fold--hideshow-empty-line-p base-indent)
        (> (current-indentation) base-indent)))

  (defun thy/fold--hideshow-seek (start direction before skip predicate base-indent)
    "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
    (save-excursion
      (goto-char start)
      (goto-char (line-beginning-position))
      (let ((bnd (if (> 0 direction)
                     (point-min)
                   (point-max)))
            (pt (point)))
        (when skip (forward-line direction))
        (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
                 do (progn
                      (when before (setq pt (line-beginning-position)))
                      (forward-line direction)
                      (unless before (setq pt (line-beginning-position)))))
        pt)))

  (defun thy/fold-hideshow-indent-range (&optional point)
    "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
    (save-excursion
      (when point
        (goto-char point))
      (let ((base-indent (current-indentation))
            (begin (point))
            (end (point)))
        (setq begin (thy/fold--hideshow-seek begin -1 t nil #'thy/fold--hideshow-geq-or-empty-p base-indent)
              begin (thy/fold--hideshow-seek begin 1 nil nil #'thy/fold--hideshow-g-or-empty-p base-indent)
              end   (thy/fold--hideshow-seek end 1 t nil #'thy/fold--hideshow-geq-or-empty-p base-indent)
              end   (thy/fold--hideshow-seek end -1 nil nil #'thy/fold--hideshow-empty-line-p base-indent))
        (list begin end base-indent))))

  (defun thy/fold-hideshow-forward-block-by-indent-fn (_arg)
    "Move forward over one indentation-based block for hideshow."
    (let ((start (current-indentation)))
      (forward-line)
      (unless (= start (current-indentation))
        (let ((range (thy/fold-hideshow-indent-range)))
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
               '(yaml-ts-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                              ""
                              "#"
                              thy/fold-hideshow-forward-block-by-indent-fn nil))
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
  :ensure t
  :preface
  (defun thy/er-mark-emt-word ()
    "Mark the Chinese word at point using `emt' segmentation."
    (interactive)
    (when (and (bound-and-true-p emt-mode)
               (fboundp 'emt--get-bounds-at-point)
               (fboundp 'emt-split))
      (condition-case nil
          (pcase-let* ((`(,beg . ,end) (emt--get-bounds-at-point 'all))
                       (index (- (point) beg))
                       (text (buffer-substring-no-properties beg end))
                       (word-bounds
                        (catch 'word-bounds
                          (dolist (bounds (emt-split text))
                            (let ((word-beg (car bounds))
                                  (word-end (cdr bounds)))
                              (when (or (and (<= word-beg index) (< index word-end))
                                        (and (< word-beg index) (<= index word-end)))
                                (throw 'word-bounds bounds)))))))
            (when word-bounds
              (goto-char (+ beg (cdr word-bounds)))
              (set-mark (point))
              (goto-char (+ beg (car word-bounds)))))
        (error nil))))
  :config
  (add-to-list 'er/try-expand-list #'thy/er-mark-emt-word)
  :bind
  ("C-l" . er/expand-region)
  ("C-M-l" . er/contract-region))
