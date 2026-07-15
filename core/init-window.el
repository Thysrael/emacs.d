;;; -*- lexical-binding: t -*-

;; Prefer horizontal splits and manage a few special buffers with built-in rules.
(use-package window
  :ensure nil
  :preface
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
  :bind (("C-c T" . thy/window-transient)
         ("C-x t" . thy/window-transient))
  :custom
  (split-width-threshold 1)
  (display-buffer-alist
   '(("\\*Ibuffer\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.5))
     ("\\*org-roam\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.5)))))

;; Select windows quickly.
(use-package ace-window
  :ensure t
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  :bind
  ("C-x o" . ace-window)
  ("C-x 9" . ace-delete-window)
  ("C-x 8" . ace-swap-window)
  :hook
  ((window-configuration-change . aw-update)) ; For modeline.
  :custom
  (aw-scope 'frame)
  (aw-background nil)
  (aw-ignore-current t)
  :config
  ;; Select windows via `M-1'...`M-9'.
  (defun thy/ace-window-select-numbered (number)
    "Select the specified window."
    (let* ((window-list (aw-window-list))
           (target-window nil))
      (cl-loop for win in window-list
               when (and (window-live-p win)
                         (eq number
                             (string-to-number
                              (window-parameter win 'ace-window-path))))
               do (setq target-window win)
               finally return target-window)

      ;; Select the target window if found.
      (if target-window
          (aw-switch-to-window target-window)
        (message "No specified window: %d" number))))
  (dotimes (n 9)
    (bind-key (concat "M-" (number-to-string (1+ n)))
              (lambda ()
                (interactive)
                (thy/ace-window-select-numbered (1+ n)))))
  )

;; Restore previous window layouts.
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :custom
  (winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode)
  :config
  (setq winner-boring-buffers
        '("*Completions*" "*Compile-Log*" "*inferior-lisp*" "*Fuzzy Completions*"
          "*Apropos*" "*Help*" "*cvs*" "*Buffer List*" "*Ibuffer*"
          "*esh command on file*"))
  )

;; Let `display-buffer-alist' control the Ibuffer window placement.
(use-package ibuffer
  :ensure nil
  :init
  (setq ibuffer-use-other-window t))

;; Keep temporary buffers in a reusable popup window.
(use-package popper
  :ensure t
  :hook
  (emacs-startup . popper-mode)
  :custom
  (popper-window-height
   (lambda (window)
     (let ((height (floor (* 0.45 (frame-height (window-frame window))))))
       (fit-window-to-buffer window height height))))
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$" "\\*Pp Eval Output\\*$"
     "\\*Compile-Log\\*"
     "\\*Completions\\*"
     "\\*Warnings\\*"
     "\\*Async Shell Command\\*"
     "\\*Apropos\\*"
     "\\*Backtrace\\*"
     "\\*Calendar\\*"
     ;; "\\*Embark Actions\\*"
     "\\*Finder\\*"
     "\\*Kill Ring\\*"
     "\\*Go-Translate\\*"
     "\\*eww bookmarks\\*"
     "\\*Outline:.*\\*.*$"
     pdf-outline-buffer-mode
     pdf-occur-buffer-mode
     "\\*toc\\*" toc-mode
     comint-mode
     compilation-mode
     ibuffer-mode
     help-mode
     tabulated-list-mode
     Buffer-menu-mode
     "\\*esup\\*"
     gnus-article-mode
     grep-mode occur-mode rg-mode

     "^\\*Process List\\*" process-menu-mode
     list-environment-mode cargo-process-mode

     "^\\*eshell.*\\*.*$" eshell-mode
     "^\\*shell.*\\*.*$"  shell-mode
     "^\\*terminal.*\\*.*$" term-mode
     "^\\*ghostel.*\\*.*$"  ghostel-mode
     "^\\*eldoc.*\\*.*$" eldoc-mode
     "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
     "\\*package update results\\*$" "\\*Package-Lint\\*$"
     "\\*[Wo]*Man.*\\*$"
     "\\*ert\\*$"
     "\\*gud-debug\\*$"
     "\\*quickrun\\*$"
     "\\*tldr\\*$"
     "\\*vc-.*\\*$"
     "^\\*elfeed-entry\\*$"
     "^\\*macro expansion\\**"

     "\\*Agenda Commands\\*"
     "\\*Org Select\\*"
     "\\*Capture\\*"
     "\\*ChatGPT\\*"
     "\\*IPADS\\*"
     "\\*chat.*"
     agent-shell-mode
     ;; "\\*Org Agenda\\*"
     "^CAPTURE-.*\\.org*"

     image-mode
     helpful-mode
     "\\*docker-.+\\*"
     "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
     chatgpt-shell-mode
	 ))

  :config
  ;; Mode-line indicator.
  (with-eval-after-load 'popper
    (setq popper-mode-line
          '(:eval `(:propertize " POP |"
                                face (:inherit ,(+mode-line-get-window-name-face)
                                               :inverse-video ,(mode-line-window-selected-p))))))

  ;; Enable indicator in minibuffer.
  (popper-echo-mode 1)

  ;; Make `C-g' close an open popup before quitting other transient state.
  (defun thy/popper-close-window-on-keyboard-quit (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'thy/popper-close-window-on-keyboard-quit)
  )


;; Resize other windows automatically with the selected window.
;; This tends to enlarge popup windows, which is difficult to avoid.
;; (use-package zoom
;;   :straight t
;;   :hook
;;   (window-setup . zoom-mode)
;;   :custom
;;   ;; (zoom-ignored-buffer-names '("*Agenda Commands*" "*vterm*"))
;;   ;; (zoom-ignored-major-modes '('chatgpt-shell-mode 'vterm-mode))
;;   )

;; Dim inactive windows.
(use-package auto-dim-other-buffers
  :ensure t
  :unless thy/on-server
  :hook ((after-init . auto-dim-other-buffers-mode)
         (auto-dim-other-buffers-mode . thy/auto-dim-other-buffers-auto-set-face))
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out t
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (defun thy/auto-dim-other-buffers-auto-set-face (&rest _)
    "Update the inactive-window dimming face after theme changes."
    (set-face-background 'auto-dim-other-buffers-face (face-background 'mode-line)))
  (advice-add #'enable-theme :after #'thy/auto-dim-other-buffers-auto-set-face)
  )

;; Highlight jump targets and copied text briefly.
(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit isearch :extend t))))
  (pulse-highlight-face ((t (:inherit lazy-highlight :extend t))))
  :hook
  (((dumb-jump-after-jump imenu-after-jump) . thy/recenter-and-pulse)
   ((bookmark-after-jump magit-diff-visit-file next-error) . thy/recenter-and-pulse-line))
  :init
  (defun thy/pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun thy/pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (thy/pulse-momentary-line)))

  (defun thy/pulse-region (beg end &rest _)
    "Pulse region between BEG and END."
    (when (and (number-or-marker-p beg)
               (number-or-marker-p end)
               (/= beg end))
      (pulse-momentary-highlight-region beg end)))

  (defun thy/recenter-and-pulse (&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (thy/pulse-momentary))

  (defun thy/recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (thy/pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer
                 aw-select thy/ace-window-select-numbered toggle-window-split
                 windmove-do-window-select
                 pager-page-down pager-page-up
                 treemacs-select-window))
    (advice-add cmd :after #'thy/pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'thy/recenter-and-pulse))

  (dolist (cmd '(symbol-overlay-basic-jump
                 compile-goto-error))
    (advice-add cmd :after #'thy/recenter-and-pulse-line))

  ;; Flash copied regions for both Emacs copy commands and Evil yanks.
  (advice-add #'kill-ring-save :after #'thy/pulse-region)

  (with-eval-after-load 'evil
    (advice-add #'evil-yank :after #'thy/pulse-region))
  (setq pulse-delay 0.05
        pulse-iterations 5)
  )
