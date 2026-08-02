;;; -*- lexical-binding: t; -*-

(use-package transient
  :ensure nil
  :preface
  (transient-define-prefix thy/brr-transient ()
    "Transient for bookmarks, registers, and rectangles."
    [["Bookmark"
      ("v" "List" list-bookmarks)
      ("M" "Mark" bookmark-set-no-overwrite)
      ("b" "Jump" bookmark-jump)]
     ["Register"
      ("l" "List" consult-register)
      ("SPC" "Point" point-to-register)
      ("s" "Text" copy-to-register)
      ("r" "Rectangle" copy-rectangle-to-register)
      ("W" "Window" window-configuration-to-register)
      ("K" "Kmacro" kmacro-to-register)]
     ["Rectangle"
      ("m" "Mark" rectangle-mark-mode)
      ("i" "Index" rectangle-number-lines)
      ("t" "String" string-rectangle)
      ("o" "Space" open-rectangle)
      ("c" "Clear" clear-rectangle)
      ("k" "Kill" kill-rectangle)
      ("y" "Yank" yank-rectangle)]])
  :bind ("C-x r" . thy/brr-transient))

(use-package evil
  :ensure t
  :demand t
  :preface
  (defun thy/evil-paste-pop-or-consult-yank-pop ()
    "Use Consult yank history in minibuffers, otherwise use Evil paste-pop."
    (interactive)
    (if (minibufferp)
	    (call-interactively #'consult-yank-pop)
      (call-interactively #'evil-paste-pop)))

  (defun thy/evil-yank-keep-point (orig-fn &rest args)
    "Call ORIG-FN with ARGS, then restore point after Evil yanks text."
    (let ((pos (copy-marker (point))))
      (unwind-protect
          (apply orig-fn args)
	    (when (marker-buffer pos)
          (goto-char pos))
	    (set-marker pos nil))))

  (defvar-local thy/evil-operator-line-number-overlays nil
    "Overlays showing relative line numbers for a pending Evil operator.")

  (defun thy/evil-hide-operator-line-numbers ()
    "Remove relative line number overlays for a pending Evil operator."
    (mapc #'delete-overlay thy/evil-operator-line-number-overlays)
    (setq thy/evil-operator-line-number-overlays nil))

  (defun thy/evil-show-operator-line-numbers ()
    "Overlay visible line starts with relative numbers while awaiting a motion."
    (thy/evil-hide-operator-line-numbers)
    (let* ((current-line (line-number-at-pos))
           (window (get-buffer-window (current-buffer)))
           (start (if window (window-start window) (point-min)))
           (end (if window
                    (or (window-end window t) (point-max))
                  (point-max))))
      (save-excursion
	    (goto-char start)
	    (let* ((line (line-number-at-pos (line-beginning-position)))
               done)
          (beginning-of-line)
          (while (not done)
            (let ((distance (abs (- line current-line))))
              (unless (zerop distance)
		        (let* ((beg (point))
                       (line-end (line-end-position))
                       (label (propertize (number-to-string distance)
                                          'face 'avy-lead-face))
                       overlay)
                  (cond
                   ((= beg (point-max))
                    (setq overlay (make-overlay beg beg))
                    (overlay-put overlay 'after-string label))
                   ((= beg line-end)
                    (setq overlay (make-overlay beg (1+ beg)))
                    (overlay-put overlay 'display (concat label "\n")))
                   (t
                    (let ((covered-width 0)
                          (label-width (string-width label))
                          (overlay-end beg))
                      (while (and (< covered-width label-width)
                                  (< overlay-end line-end))
			            (let ((char (char-after overlay-end)))
                          (setq covered-width
				                (+ covered-width
                                   (if (eq char ?\t)
                                       (- tab-width (% covered-width tab-width))
                                     (or (char-width char) 1)))))
			            (setq overlay-end (1+ overlay-end)))
                      (setq overlay (make-overlay beg overlay-end))
                      (overlay-put overlay 'display
                                   (concat label
                                           (make-string
                                            (max 0 (- covered-width label-width))
                                            ?\s))))))
                  (when window
                    (overlay-put overlay 'window window))
                  (overlay-put overlay 'priority 100)
                  (push overlay thy/evil-operator-line-number-overlays))))
            (setq line (1+ line))
            (setq done (or (>= (line-end-position) end)
                           (not (zerop (forward-line 1))))))))))

  (defun thy/section-heading-regexp ()
    "Return a heading regexp for document section text objects."
    (cond
     ((derived-mode-p 'org-mode) "^\\*+\\(?:[ \\t]\\|$\\)")
     ((derived-mode-p 'markdown-mode 'markdown-ts-mode) "^#+\\(?:[ \\t]\\|$\\)")))

  (defun thy/section-heading-level ()
    "Return heading level at point for document section text objects."
    (save-excursion
      (back-to-indentation)
      (cond
       ((looking-at "\\*+") (length (match-string 0)))
       ((looking-at "#+") (length (match-string 0)))
       (t 0))))

  (defun thy/section-bounds (&optional inner)
    "Return current Org/Markdown section bounds.
When INNER is non-nil, exclude the heading line."
    (when-let* ((heading-regexp (thy/section-heading-regexp)))
      (save-excursion
	    (let (start content-start end level)
          (unless (looking-at heading-regexp)
            (re-search-backward heading-regexp nil t))
          (if (looking-at heading-regexp)
              (setq start (line-beginning-position)
                    content-start (line-beginning-position 2)
                    level (thy/section-heading-level))
            (setq start (point-min)
                  content-start (point-min)
                  level 0))
          (goto-char content-start)
          (setq end
		        (if (= level 0)
                    (or (and (re-search-forward heading-regexp nil t)
                             (line-beginning-position))
			            (point-max))
                  (catch 'section-end
                    (while (re-search-forward heading-regexp nil t)
                      (when (<= (thy/section-heading-level) level)
			            (throw 'section-end (line-beginning-position))))
                    (point-max))))
          (cons (if inner content-start start)
		        (max (if inner content-start start) end))))))

  (defconst thy/evil-mode-specific-command-overrides
    '(((org-mode)
       (?a . thy/org-archive-subtree-hierarchical))
      ((markdown-ts-mode markdown-ts-view-mode gfm-view-mode)
       (?o . markdown-follow-thing-at-point)
       (?v . thy/toggle-markdown-mode))
      ((markdown-mode)
       (?v . thy/toggle-markdown-mode)))
    "Mode-specific exceptions to automatic SPC c key forwarding.")

  (defun thy/evil-prefix-override (event overrides)
    "Return the mode-specific override for EVENT from OVERRIDES."
    (cl-loop for (modes . bindings) in overrides
             when (or (null modes) (apply #'derived-mode-p modes))
             thereis (alist-get event bindings)))

  (defun thy/evil-active-key-binding (keys)
    "Return the first complete binding for KEYS in active maps."
    (cl-loop for map in (current-active-maps t)
             for binding = (lookup-key map keys)
             when (and binding
                       (not (numberp binding))
                       (not (eq binding 'undefined)))
             return (if (eq (car-safe binding) 'menu-item)
			            (nth 2 binding)
                      binding)))

  (defun thy/evil-make-prefix-map (command)
    "Return a printable-key prefix map dispatching to COMMAND."
    (let ((map (make-keymap)))
      (set-char-table-range (cadr map) '(32 . 126) command)
      map))

  (defun thy/evil-prefix-command-binding (event control overrides)
    "Resolve EVENT after C-c, adding CONTROL unless it is nil.
OVERRIDES contains mode-specific exceptions checked before active keymaps."
    (or (thy/evil-prefix-override event overrides)
	    (thy/evil-active-key-binding
	     (vector ?\C-c
		         (if control
                     (event-convert-list (list 'control event))
                   event)))))

  (defun thy/evil-execute-prefix-binding (binding event description)
    "Execute BINDING for EVENT, reporting it under DESCRIPTION."
    (setq binding (if (symbolp binding)
                      (or (command-remapping binding) binding)
                    binding))
    (cond
     ((keymapp binding)
      (set-transient-map binding)
      (message "%s%s-" description (key-description (vector event))))
     ((commandp binding)
      (command-execute binding 'record))
     (t
      (user-error "No command is bound to %s%s"
                  description (key-description (vector event))))))

  (defun thy/evil-mode-specific-command-binding (event)
    "Return the command for EVENT under the current SPC c prefix."
    (thy/evil-prefix-command-binding
     event t thy/evil-mode-specific-command-overrides))

  (defun thy/evil-mode-specific-command ()
    "Run the current C-c C-* command from the SPC c * equivalent."
    (interactive)
    (thy/evil-execute-prefix-binding
     (thy/evil-mode-specific-command-binding last-command-event)
     last-command-event "C-c C-"))

  (defvar thy/evil-mode-specific-command-map
    (thy/evil-make-prefix-map #'thy/evil-mode-specific-command)
    "Dynamic SPC c prefix map forwarding to C-c C-* commands.")

  (defvar-keymap thy/evil-other-window-map
    :doc "SPC 4 prefix map for other-window commands."
    "f" #'find-file-other-window
    "b" #'switch-to-buffer-other-window)

  (defconst thy/evil-leader-command-overrides
    '(((org-agenda-mode)
       (?w . org-save-all-org-buffers))
      (nil
       (?0 . delete-window)
       (?1 . delete-other-windows)
       (?2 . split-window-below)
       (?3 . split-window-right)
       (?8 . ace-swap-window)
       (?9 . ace-delete-window)
       (?b . switch-to-buffer)
       (?B . switch-to-buffer-other-window)
       (?f . find-file)
       (?o . ace-window)
       (?O . thy/agent-shell-transient)
       (?r . consult-recent-file)
       (?R . thy/brr-transient)
       (?w . save-buffer)))
    "Mode-specific exceptions to automatic SPC key forwarding.")

  (defun thy/evil-leader-command ()
    "Run the current C-c * command from the SPC * equivalent."
    (interactive)
    (thy/evil-execute-prefix-binding
     (thy/evil-prefix-command-binding
      last-command-event nil thy/evil-leader-command-overrides)
     last-command-event "C-c "))

  (defvar thy/evil-leader-command-map
    (let ((map (thy/evil-make-prefix-map #'thy/evil-leader-command)))
      (define-key map (kbd "4") thy/evil-other-window-map)
      (define-key map (kbd "c") thy/evil-mode-specific-command-map)
      map)
    "Dynamic SPC prefix map forwarding to C-c * commands.")

  (defun thy/evil-bind-local-leader ()
    "Bind the leader above mode-specific maps in the current buffer."
    (when evil-local-mode
      (evil-local-set-key 'normal (kbd "SPC") thy/evil-leader-command-map)
      (evil-local-set-key 'motion (kbd "SPC") thy/evil-leader-command-map)))

  :init
  (setq evil-respect-visual-line-mode nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; Evil 1.15 declares this variable without giving it an initial value.
  (setq evil-mode-buffers nil)
  :config
  (setq evil-symbol-word-search t)
  (setq evil-want-fine-undo t)
  (add-hook 'evil-local-mode-hook #'thy/evil-bind-local-leader)
  (evil-mode 1)

  ;; Move by display lines, while operator motions keep logical line semantics.
  (evil-define-minor-mode-key '(normal visual) 'visual-line-mode
    (kbd "j") #'evil-next-visual-line
    (kbd "k") #'evil-previous-visual-line
    (kbd "g j") #'evil-next-line
    (kbd "g k") #'evil-previous-line)

  ;; Keep yanks visually stable; the pulse feedback already shows what was copied.
  (advice-add #'evil-yank :around #'thy/evil-yank-keep-point)
  (add-hook 'evil-operator-state-entry-hook #'thy/evil-show-operator-line-numbers)
  (add-hook 'evil-operator-state-exit-hook #'thy/evil-hide-operator-line-numbers)

  (evil-define-operator thy/evil-format (beg end type)
    "Format text from BEG to END using Evil motion TYPE."
    (interactive "<R>")
    (when (eq type 'line)
      (setq end (save-excursion
                  (goto-char end)
                  (line-end-position))))
    (thy/format-region-or-buffer beg end))

  (defun thy/yank-media-image-available-p ()
    "Return non-nil when the clipboard contains a supported image."
    (and (require 'yank-media nil t)
         (ignore-errors (yank-media--find-matching-media "image/.*"))))

  (evil-define-command thy/evil-paste-after
      (count &optional register yank-handler)
    "Yank a clipboard image when supported, otherwise paste with Evil."
    :suppress-operator t
    (interactive "*P<x>")
    (if (and (= (prefix-numeric-value count) 1)
             (not register)
             (not (evil-visual-state-p))
             (thy/yank-media-image-available-p))
        (condition-case nil
            (progn
              (when (and (derived-mode-p 'org-mode)
                         (null yank-media--registered-handlers))
                (org-setup-yank-dnd-handlers))
              (call-interactively #'yank-media))
          (user-error
           (evil-paste-after count register yank-handler)))
      (evil-paste-after count register yank-handler)))

  (evil-define-text-object thy/evil-inner-section (count &optional beg end type)
    "Select the current Org/Markdown section body."
    :type line
    (when-let* ((bounds (thy/section-bounds t)))
      (evil-range (car bounds) (cdr bounds) 'line)))

  (evil-define-text-object thy/evil-a-section (count &optional beg end type)
    "Select the current Org/Markdown section including its heading."
    :type line
    (when-let* ((bounds (thy/section-bounds)))
      (evil-range (car bounds) (cdr bounds) 'line)))

  (defun thy/latex-environment-bounds ()
    "Return outer and inner bounds of the enclosing LaTeX environment."
    (unless (derived-mode-p 'LaTeX-mode)
      (user-error "Not in an AUCTeX LaTeX buffer"))
    (save-excursion
      (unless (looking-at (regexp-quote "\\begin{"))
        ;; AUCTeX needs point away from the closing brace of an environment.
        (when (or (eq (char-before) ?}) (eq (char-after) ?}))
          (backward-char 2))
        (LaTeX-find-matching-begin))
      (let ((outer-beg (point))
            outer-end inner-beg inner-end)
        (forward-sexp)
        (while (memq (char-after) '(?{ ?\[))
          (forward-sexp))
        (when (looking-at "\n[ \t]*")
          (goto-char (match-end 0)))
        (setq inner-beg (point))
        (goto-char (1+ outer-beg))
        (LaTeX-find-matching-end)
        (setq outer-end (point))
        (search-backward "\\end")
        (when (looking-back "\n[ \t]*" (line-beginning-position 0))
          (goto-char (match-beginning 0)))
        (setq inner-end (point))
        (list outer-beg outer-end inner-beg inner-end))))

  (evil-define-text-object thy/evil-inner-latex-environment
      (count &optional beg end type)
    "Select inside the enclosing LaTeX environment."
    :extend-selection nil
    (last (thy/latex-environment-bounds) 2))

  (evil-define-text-object thy/evil-a-latex-environment
      (count &optional beg end type)
    "Select the enclosing LaTeX environment including delimiters."
    :extend-selection nil
    (nbutlast (thy/latex-environment-bounds) 2))

  (defun thy/evil-select-pair (fallback open close count beg end type inclusive)
    "Select OPEN and CLOSE around point, or call FALLBACK.
COUNT, BEG, END, TYPE, and INCLUSIVE follow `evil-select-paren'."
    (let* ((origin (point))
           (outer (ignore-errors
                    (evil-select-paren open close beg end type count t))))
      (if (and outer
               (<= (car outer) origin)
               (<= origin (cadr outer)))
          (evil-select-paren open close beg end type count inclusive)
        (funcall fallback count beg end type))))

  (defmacro thy/evil-define-pair-text-objects
      (name fallback-inner fallback-outer open close keys description
            &optional outer-extend)
    "Define paired Evil text objects NAME for OPEN, CLOSE, and KEYS."
    (let ((inner (intern (format "thy/evil-inner-%s" name)))
          (outer (intern (format "thy/evil-a-%s" name))))
      `(progn
         (evil-define-text-object ,inner (count &optional beg end type)
           ,(format "Select inside ASCII or Chinese %s." description)
           :extend-selection nil
           (thy/evil-select-pair #',fallback-inner ,open ,close
                                 count beg end type nil))
         (evil-define-text-object ,outer (count &optional beg end type)
           ,(format "Select around ASCII or Chinese %s." description)
           :extend-selection ,outer-extend
           (thy/evil-select-pair #',fallback-outer ,open ,close
                                 count beg end type t))
         (dolist (key ',keys)
           (define-key evil-inner-text-objects-map key #',inner)
           (define-key evil-outer-text-objects-map key #',outer)))))

  (thy/evil-define-pair-text-objects
   paren evil-inner-paren evil-a-paren ?（ ?） ("b" "(" ")") "parentheses")
  (thy/evil-define-pair-text-objects
   bracket evil-inner-bracket evil-a-bracket ?【 ?】 ("[" "]") "brackets")
  (thy/evil-define-pair-text-objects
   angle evil-inner-angle evil-a-angle ?《 ?》 ("<" ">") "book-title brackets")
  (thy/evil-define-pair-text-objects
   double-quote evil-inner-double-quote evil-a-double-quote ?“ ?” ("\"") "quotes" t)

  (evil-set-initial-state 'color-rg-mode 'motion)
  (evil-set-initial-state 'ghostel-mode 'insert)
  (evil-set-initial-state 'help-mode 'normal)

  (define-key evil-inner-text-objects-map "s" #'thy/evil-inner-section)
  (define-key evil-outer-text-objects-map "s" #'thy/evil-a-section)
  (define-key evil-inner-text-objects-map "e" #'thy/evil-inner-latex-environment)
  (define-key evil-outer-text-objects-map "e" #'thy/evil-a-latex-environment)

  ;; In minibuffers, use Consult history instead of Evil paste-pop state checks.
  (define-key evil-normal-state-map (kbd "M-y") #'thy/evil-paste-pop-or-consult-yank-pop)
  (define-key evil-normal-state-map [remap yank-pop] #'thy/evil-paste-pop-or-consult-yank-pop)
  (define-key evil-normal-state-map "p" #'thy/evil-paste-after)

  ;; Normal-state single keys are deliberately tuned for this config, not pure Vim.
  (define-key evil-normal-state-map (kbd ";") #'embark-act)
  (define-key evil-normal-state-map (kbd "P") #'consult-yank-pop)
  (define-key evil-normal-state-map (kbd "=") #'thy/evil-format)
  (define-key evil-normal-state-map (kbd "gd") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "gr") #'xref-find-references)
  (define-key evil-normal-state-map (kbd "J") #'avy-goto-char-2)
  (define-key evil-normal-state-map (kbd "K") #'eldoc)
  (define-key evil-normal-state-map (kbd "s") #'consult-line)
  (define-key evil-normal-state-map (kbd "C-t") #'thy/ghostel-toggle-popup)
  (define-key evil-normal-state-map (kbd "H") #'mwim-beginning-of-code-or-line)
  (define-key evil-normal-state-map (kbd "L") #'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "m") #'symbol-overlay-put)
  (define-key evil-normal-state-map (kbd "M") #'symbol-overlay-remove-all)
  (define-key evil-normal-state-map (kbd "U") #'vundo)
  (define-key evil-normal-state-map (kbd "z") #'hs-toggle-hiding)
  (define-key evil-normal-state-map (kbd "Z") #'thy/hs-toggle-all)

  (define-key evil-insert-state-map (kbd "C-n") #'next-line)
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
  (define-key evil-insert-state-map (kbd "C-s") #'consult-line)
  (define-key evil-insert-state-map (kbd "C-a") #'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-e") #'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-f") #'thy/smart-forward)
  (define-key evil-insert-state-map (kbd "C-b") #'backward-char)
  (define-key evil-insert-state-map (kbd "M-<") #'beginning-of-buffer)
  (define-key evil-insert-state-map (kbd "M->") #'end-of-buffer)

  (define-key evil-visual-state-map (kbd "v") #'er/expand-region)
  (define-key evil-visual-state-map (kbd "V") #'er/contract-region)
  (define-key evil-visual-state-map (kbd "=") #'thy/evil-format)

  (define-key evil-motion-state-map (kbd "H") #'evil-beginning-of-line)
  (define-key evil-motion-state-map (kbd "L") #'evil-end-of-line)

  ;; Use C-j as a direct C-x prefix inside Evil states; key-translation-map breaks C-x.
  (dolist (map (list evil-normal-state-map
                     evil-insert-state-map
                     evil-visual-state-map
                     evil-motion-state-map
                     evil-emacs-state-map))
    (define-key map (kbd "C-j") ctl-x-map))

  (with-eval-after-load 'org
    (evil-define-key 'normal org-mode-map
      (kbd "TAB") #'org-cycle
      (kbd "<tab>") #'org-cycle))

  (with-eval-after-load 'markdown-ts-mode
    (evil-define-key 'normal markdown-ts-mode-map
      (kbd "TAB") #'markdown-ts-outline-cycle
      (kbd "<tab>") #'markdown-ts-outline-cycle))

  (with-eval-after-load 'corfu
    (when (fboundp 'corfu-quit)
      (add-hook 'evil-insert-state-exit-hook #'corfu-quit))))

(use-package evil-collection
  :ensure t
  :after evil
  :demand t
  :preface
  (defun thy/evil-collection-setup-org-agenda (mode _maps &rest _)
    "Install local navigation bindings when MODE is `org-agenda'."
    (when (eq mode 'org-agenda)
      (evil-define-key 'normal org-agenda-mode-map
        (kbd "g") #'org-agenda-redo
        (kbd "h") #'org-agenda-earlier
        (kbd "l") #'org-agenda-later)))
  :config
  (add-hook 'evil-collection-setup-hook
            #'thy/evil-collection-setup-org-agenda)
  (evil-collection-init '(magit dired org-agenda))

  (with-eval-after-load 'magit
    (with-eval-after-load 'evil-collection-magit
      (evil-define-key 'normal magit-mode-map
        (kbd "J") #'magit-section-forward-sibling
        (kbd "K") #'magit-section-backward-sibling)
      (evil-define-key 'normal magit-status-mode-map
        (kbd "g") #'magit-refresh)))

  ;; Preserve local additions after evil-collection installs its Dired bindings.
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map
      (kbd "h") #'dired-up-directory
      (kbd "l") #'dired-find-file
      (kbd "y") #'dired-do-copy
      (kbd "p") #'dirvish-yank
      (kbd "P") #'dirvish-yank-menu
      (kbd "Y") #'thy/dired-copy-files-to-clipboard
      (kbd "W") #'thy/dired-copy-files-to-clipboard)
    (evil-collection-define-key 'normal 'dired-mode-map
      (kbd "o") #'dired-do-open))

  (with-eval-after-load 'dirvish
    (dolist (binding thy/dirvish-mode-bindings)
      (evil-collection-define-key 'normal 'dirvish-mode-map
        (kbd (car binding)) (cdr binding)))))

(use-package evil-commentary
  :ensure t
  :after evil
  :commands (evil-commentary evil-commentary-line)
  :init
  (define-key evil-normal-state-map (kbd "gc") #'evil-commentary)
  (define-key evil-visual-state-map (kbd "gc") #'evil-commentary-line))

;; Add/change/delete pairs with `ys{motion}`, `cs`, and `ds`; use `S` visually.
(use-package evil-surround
  :ensure t
  :after evil
  :hook (after-init . global-evil-surround-mode))

(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :demand t
  :config
  (define-key evil-inner-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "c"
              (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-outer-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (define-key evil-inner-text-objects-map "l"
              (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "l"
              (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "/"
              (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  (define-key evil-outer-text-objects-map "/"
              (evil-textobj-tree-sitter-get-textobj "comment.outer")))
