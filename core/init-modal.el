;;; -*- lexical-binding: t; -*-

(transient-define-prefix thy/window-transient ()
  "Transient for window and tab commands."
  [["Tab"
    ("c" "Switch" tab-switch)
    ("0" "Close" tab-close)
    ("2" "New" tab-new)
    ("r" "Rename" tab-rename :transient t)]
   ["Winner"
    ("u" "Undo" winner-undo :transient t)
    ("U" "Redo" winner-redo :transient t)]
   ["Desktop"
    ("S" "Save" desktop-save-in-desktop-dir)
    ("l" "Load" desktop-read)]
   ["Resize"
    ("=" "Grow" enlarge-window :transient t)
    ("-" "Shrink" shrink-window :transient t)
    ("+" "Widen" enlarge-window-horizontally :transient t)
    ("_" "Narrow" shrink-window-horizontally :transient t)]])

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

(global-set-key (kbd "C-x t") #'thy/window-transient)
(global-set-key (kbd "C-x r") #'thy/brr-transient)

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

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; Evil 1.15 declares this variable without giving it an initial value.
  (setq evil-mode-buffers nil)
  :config
  (setq evil-symbol-word-search t)
  (setq evil-want-fine-undo t)
  (evil-mode 1)

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
    (if (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-managed-p)
             (eglot-managed-p))
        (eglot-format beg end)
      (indent-region beg end)))

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

  (evil-set-initial-state 'color-rg-mode 'motion)
  (evil-set-initial-state 'ghostel-mode 'insert)
  (evil-set-initial-state 'help-mode 'normal)

  (define-key evil-inner-text-objects-map "s" #'thy/evil-inner-section)
  (define-key evil-outer-text-objects-map "s" #'thy/evil-a-section)

  ;; In minibuffers, use Consult history instead of Evil paste-pop state checks.
  (define-key evil-normal-state-map (kbd "M-y") #'thy/evil-paste-pop-or-consult-yank-pop)
  (define-key evil-normal-state-map [remap yank-pop] #'thy/evil-paste-pop-or-consult-yank-pop)

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

  ;; Keep leader bindings explicit instead of pulling in a leader/general package.
  (evil-define-key 'normal 'global
    (kbd "SPC a") #'org-agenda
    (kbd "SPC A") #'calendar
    (kbd "SPC o") #'ace-window
    (kbd "SPC k") #'org-capture
    (kbd "SPC 9") #'ace-delete-window
    (kbd "SPC 8") #'ace-swap-window
    (kbd "SPC 0") #'delete-window
    (kbd "SPC 1") #'delete-other-windows
    (kbd "SPC 2") #'split-window-below
    (kbd "SPC 3") #'split-window-right
    (kbd "SPC 4 f") #'find-file-other-window
    (kbd "SPC 4 b") #'switch-to-buffer-other-window
    (kbd "SPC ;") #'embark-dwim
    (kbd "SPC b") #'switch-to-buffer
    (kbd "SPC B") #'switch-to-buffer-other-window
    (kbd "SPC e") #'consult-flymake
    (kbd "SPC f") #'find-file
    (kbd "SPC g") #'thy/consult-diff-hunks
    (kbd "SPC G") #'thy/diff-hunk-transient
    (kbd "SPC i") #'consult-imenu
    (kbd "SPC n") #'consult-notes
    (kbd "SPC N") #'thy/note-transient
    (kbd "SPC O") #'thy/agent-shell-transient
    (kbd "SPC p") #'consult-fd
    (kbd "SPC r") #'consult-recent-file
    (kbd "SPC R") #'thy/brr-transient
    (kbd "SPC s") #'consult-ripgrep
    (kbd "SPC t") #'thy/ghostel-transient
    (kbd "SPC T") #'thy/window-transient
    (kbd "SPC v") #'magit
    (kbd "SPC w") #'save-buffer)

  (with-eval-after-load 'org
    (evil-define-key 'normal org-mode-map
      (kbd "TAB") #'org-cycle
      (kbd "<tab>") #'org-cycle
      (kbd "SPC c a") #'thy/org-archive-subtree-hierarchical
      (kbd "SPC c c") #'org-ctrl-c-ctrl-c
      (kbd "SPC c o") #'org-open-at-point
      (kbd "SPC c s") #'org-schedule
      (kbd "SPC c t") #'org-todo
      (kbd "SPC c w") #'org-refile))

  (with-eval-after-load 'org-capture
    (evil-define-key 'normal org-capture-mode-map
      (kbd "SPC c c") #'org-capture-finalize
      (kbd "SPC c k") #'org-capture-kill))

  (with-eval-after-load 'markdown-ts-mode
    (evil-define-key 'normal markdown-ts-mode-map
      (kbd "TAB") #'markdown-ts-outline-cycle
      (kbd "<tab>") #'markdown-ts-outline-cycle
      (kbd "H") #'evil-beginning-of-visual-line
      (kbd "L") #'evil-end-of-visual-line
      (kbd "SPC c o") #'markdown-follow-thing-at-point
      (kbd "SPC c v") #'thy/toggle-markdown-mode)
    (evil-define-key 'visual markdown-ts-mode-map
      (kbd "H") #'evil-beginning-of-visual-line
      (kbd "L") #'evil-end-of-visual-line)
    (evil-define-key 'normal markdown-ts-view-mode-map
      (kbd "H") #'evil-beginning-of-visual-line
      (kbd "L") #'evil-end-of-visual-line
      (kbd "SPC c o") #'markdown-follow-thing-at-point
      (kbd "SPC c v") #'thy/toggle-markdown-mode))

  (with-eval-after-load 'markdown-mode
    (evil-define-key 'normal markdown-mode-map
      (kbd "SPC c o") #'markdown-follow-thing-at-point
      (kbd "SPC c v") #'thy/toggle-markdown-mode)
    (evil-define-key 'normal gfm-view-mode-map
      (kbd "SPC c o") #'markdown-follow-thing-at-point
      (kbd "SPC c v") #'thy/toggle-markdown-mode))

  (with-eval-after-load 'git-commit
    (evil-define-key 'normal git-commit-mode-map
      (kbd "SPC c c") #'with-editor-finish))

  (with-eval-after-load 'corfu
    (when (fboundp 'corfu-quit)
      (add-hook 'evil-insert-state-exit-hook #'corfu-quit))))

(defun thy/evil-collection-restore-org-agenda-leader (mode _maps &rest _)
  "Restore the global Evil leader when MODE is `org-agenda'."
  (when (eq mode 'org-agenda)
    (evil-define-key 'normal org-agenda-mode-map
      (kbd "SPC") (lookup-key evil-normal-state-map (kbd "SPC"))
      (kbd "g") #'org-agenda-redo
      (kbd "h") #'org-agenda-earlier
      (kbd "l") #'org-agenda-later)))

(use-package evil-collection
  :ensure t
  :after evil
  :demand t
  :config
  (add-hook 'evil-collection-setup-hook
            #'thy/evil-collection-restore-org-agenda-leader)
  (evil-collection-init '(magit dired org-agenda))

  (with-eval-after-load 'magit
    (evil-define-key 'normal magit-mode-map
      (kbd "SPC") (lookup-key evil-normal-state-map (kbd "SPC"))
      (kbd "J") #'magit-section-forward-sibling
      (kbd "K") #'magit-section-backward-sibling))

  ;; Preserve local additions after evil-collection installs its Dired bindings.
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map
      (kbd "h") #'dired-up-directory
      (kbd "l") #'dired-find-file
      (kbd "y") #'dired-do-copy
      (kbd "p") #'dirvish-yank
      (kbd "P") #'dirvish-yank-menu
      (kbd "Y") #'thy/dired-copy-files-to-clipboard
      (kbd "W") #'thy/dired-copy-files-to-clipboard))

  (with-eval-after-load 'dirvish
    (evil-define-key 'normal dirvish-mode-map
      (kbd "SPC") (lookup-key evil-normal-state-map (kbd "SPC")))
    (dolist (binding thy/dirvish-mode-bindings)
      (evil-define-key 'normal dirvish-mode-map
        (kbd (car binding)) (cdr binding)))))

(use-package evil-commentary
  :ensure t
  :after evil
  :commands (evil-commentary evil-commentary-line)
  :init
  (define-key evil-normal-state-map (kbd "gc") #'evil-commentary)
  (define-key evil-visual-state-map (kbd "gc") #'evil-commentary-line))

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
