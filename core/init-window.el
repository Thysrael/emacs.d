;;; -*- lexical-binding: t -*-

;; window scroll 绑定
(defvar +scrolling-lines 5)
(bind-keys*
 ;; 在其他窗口滚动
 ("M-<down>" . (lambda () (interactive) (scroll-other-window +scrolling-lines)))
 ("M-<up>" . (lambda () (interactive) (scroll-other-window-down +scrolling-lines)))
 ;; 在本窗口滚动
 ("C-v" . (lambda () (interactive) (scroll-up +scrolling-lines)))
 ("C-M-v" . (lambda () (interactive) (scroll-up (- +scrolling-lines))))
 )

;; 弹出窗口时使用左右分屏
(setq split-width-threshold 1)

;; 选择窗口
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
  ((window-configuration-change . aw-update)) ;; For modeline
  :config
  (setq aw-scope 'frame
        aw-background nil
        aw-ignore-current t)
  ;; Select widnow via `M-1'...`M-9'
  (defun +aw--select-window (number)
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

      ;; Select the target window if found
      (if target-window
          (aw-switch-to-window target-window)
        (message "No specified window: %d" number))))
  (dotimes (n 9)
    (bind-key (concat "M-" (number-to-string (1+ n)))
              (lambda ()
                (interactive)
                (+aw--select-window (1+ n)))))
  )

;; 存储原来的窗口布局
(use-package winner
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

;; 控制弹出窗口的行为
;; https://depp.brause.cc/shackle/
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below)  ; default below
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '(
          ;; ("\\*Org Agenda.*\\*" :regexp t :select t :size 1 :same t)
          ("\\*Ibuffer\\*" :regexp t :select t :size 0.5 :align below)
          ("\\*org-roam\\*" :regexp t :select t :size 0.5 :align below)
          ))
  )

;; popper 是一种特殊 buffer，所有的 popper 都只会占用同一个 window （出现在底部）
;; 利用这种机制可以使我们将许多临时的 buffer 都管理在一个窗口下
(use-package popper
  :ensure t
  :bind
  ("C-=" . popper-toggle-type) ; 将 popper 转换为普通 buffer
  ("C--"  . popper-cycle) ; 切换多个 popper, 也可用于 toggle 出 popper window
  :hook
  (emacs-startup . popper-mode)
  :custom
  (popper-window-height 18)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*color-rg\\*"
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
     "\\*maple-translate\\*"
     "\\*eww bookmarks\\*"
     "\\*Outline:.*\\*.*$"
     pdf-outline-buffer-mode
     pdf-occur-buffer-mode
     ;; "Bookmark List" bookmark-bmenu-mode
     "\\*toc\\*" toc-mode
     comint-mode
     compilation-mode
     ibuffer-mode
     help-mode
     tabulated-list-mode
     Buffer-menu-mode
     "\\*esup\\*"
     gnus-article-mode devdocs-mode
     grep-mode occur-mode rg-mode ag-mode pt-mode
     osx-dictionary-mode

     "^\\*Process List\\*" process-menu-mode
     list-environment-mode cargo-process-mode

     "^\\*eshell.*\\*.*$" eshell-mode
     "^\\*shell.*\\*.*$"  shell-mode
     "^\\*terminal.*\\*.*$" term-mode
     "^\\*vterm.*\\*.*$"  vterm-mode
     "^\\*eldoc.*\\*.*$" eldoc-mode
     ;; "^magit.*$" magit-mode
     "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
     "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
     "\\*[Wo]*Man.*\\*$"
     "\\*ert\\*$" overseer-buffer-mode
     "\\*gud-debug\\*$"
     "\\*lsp-help\\*$" "\\*lsp session\\*$"
     "\\*quickrun\\*$"
     "\\*tldr\\*$"
     "\\*vc-.*\\*$"
     "^\\*elfeed-entry\\*$"
     "^\\*macro expansion\\**"

     "\\*Agenda Commands\\*"
     "\\*Org Select\\*"
     ;; "\\*org-roam\\*" ;; 因为 org9.7 报错 (setq warning-suppress-types '((org-element org-element-parser)))
     "\\*Capture\\*"
     "\\*ChatGPT\\*"
     "\\*IPADS\\*"
     "\\*chat.*"
     ;; "\\*Org Agenda\\*"
     "^CAPTURE-.*\\.org*"

     image-mode
     helpful-mode
     "\\*docker-.+\\*"
     "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
     "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
     rustic-cargo-outdated-mode rustic-cargo-test-mode

     chatgpt-shell-mode
	 ))

  :config
  ;; mode-line indicator
  (with-eval-after-load 'popper
    (setq popper-mode-line
          '(:eval `(:propertize " POP |"
                                face (:inherit ,(+mode-line-get-window-name-face)
                                               :inverse-video ,(mode-line-window-selected-p))))))

  ;; Enable indicator in minibuffer
  (popper-echo-mode 1)

  ;; HACK: close popper with `C-g'
  (defun +popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'+popper-close-window-hack)
  )


;; 可以随着所在 window 调整其他 window 的大小
;; 会导致 pop 提高，这是无法避免的
;; (use-package zoom
;;   :straight t
;;   :hook
;;   (window-setup . zoom-mode)
;;   :custom
;;   ;; (zoom-ignored-buffer-names '("*Agenda Commands*" "*vterm*"))
;;   ;; (zoom-ignored-major-modes '('chatgpt-shell-mode 'vterm-mode))
;;   )

;; 不活跃的 window 会变暗
(use-package auto-dim-other-buffers
  :ensure t
  :hook ((after-init . auto-dim-other-buffers-mode)
         (auto-dim-other-buffers-mode . +auto-dim-other-buffers-auto-set-face))
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out t
        auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)

  (defadvice! +auto-dim-other-buffers-auto-set-face (&rest _)
    :after #'enable-theme
    (set-face-background 'auto-dim-other-buffers-face (face-background 'mode-line))
    )
  )

;; Highlight line at cursor after switching window
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t))))
  :hook
  (((dumb-jump-after-jump imenu-after-jump) . +recenter-and-pulse)
   ((bookmark-after-jump magit-diff-visit-file next-error) . +recenter-and-pulse-line))
  :init
  (defun +pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun +pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (+pulse-momentary-line)))

  (defun +recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (+pulse-momentary))

  (defun +recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (+pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window switch-to-buffer
                 aw-select +aw--select-window toggle-window-split
                 windmove-do-window-select
                 pager-page-down pager-page-up
                 treemacs-select-window))
    (advice-add cmd :after #'+pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'+recenter-and-pulse))

  (dolist (cmd '(symbol-overlay-basic-jump
                 compile-goto-error))
    (advice-add cmd :after #'+recenter-and-pulse-line))
  (setq pulse-delay 0.04
        pulse-iterations 4)
  )
