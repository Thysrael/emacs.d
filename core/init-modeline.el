;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))

;; show encodings for UTF-8:LF
(defvar +mode-line-show-common-encodings nil)
;; show VC tools name for Git
(defvar +mode-line-show-common-vc-tools-name nil)

;;; Window width
(defconst +mode-line-window-width-limit 90)
(defvar-local +mode-line-enough-width-p nil)
(defun +mode-line-window-size-change-function (&rest _)
  "Update whether the selected window is wide enough for full details."
  (setq +mode-line-enough-width-p
        (> (window-total-width) +mode-line-window-width-limit)))

;;; Faces
(defgroup +mode-line nil
  "Mode-Line faces."
  :group 'faces)

(defface +mode-line-meta-inactive-unchanged-face
  '((t (:inherit (font-lock-function-name-face bold))))
  "The face for meta panel on the mode-line of an inactive window."
  :group '+mode-line)

(defface +mode-line-meta-inactive-modified-face
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face used for meta panel on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-meta-inactive-autosaved-face
  '((t (:inherit (font-lock-doc-face bold))))
  "Face used for meta panel on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-mode-name-active-face
  '((t (:inherit (font-lock-function-name-face))))
  "The face for buffer name on the mode-line of an active window."
  :group '+mode-line)

(defface +mode-line-host-name-active-face
  '((t (:inherit (font-lock-function-name-face bold italic))))
  "The face for host name on the mode-line of an active window."
  :group '+mode-line)

;;; Indicators
(defsubst +mode-line-get-window-name ()
  "Get window name for current window."
  (concat " " (window-parameter (selected-window) 'ace-window-path)))

(defsubst +mode-line-get-window-name-face ()
  "Get face of window name for current window."
  (let ((modified (buffer-modified-p)))
    (cond ((eq modified t)
           '+mode-line-meta-inactive-modified-face)
          ((eq modified nil)
           '+mode-line-meta-inactive-unchanged-face)
          ((eq modified 'autosaved)
           '+mode-line-meta-inactive-autosaved-face))))

(defsubst +mode-line-macro-indicator ()
  "Display current Emacs macro being recorded."
  (cond (defining-kbd-macro "| MacroDef ")
        (executing-kbd-macro "| MacroExc ")))

(defun +mode-line-count-words (beg end)
  "Count words between BEG and END, using `emt-split' when available."
  (or (when (fboundp 'emt-split)
        (ignore-errors
          (length (emt-split (buffer-substring-no-properties beg end)))))
      (count-words beg end)))

(defvar-local +mode-line-region-cache-key nil)
(defvar-local +mode-line-region-cache-value nil)

(defun +mode-line-region-key (beg end)
  "Return cache key for region stats between BEG and END."
  (list beg end (point) (mark t) (buffer-chars-modified-tick)
        (and (boundp 'emt--lib-loaded) emt--lib-loaded)))

(defun +mode-line-use-region-indicator ()
  "Display selected region in current buffer."
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (key (+mode-line-region-key beg end)))
      (unless (equal key +mode-line-region-cache-key)
        (setq +mode-line-region-cache-key key
              +mode-line-region-cache-value
              (concat "| L" (number-to-string (count-lines beg end))
                      " C" (number-to-string (abs (- (mark t) (point))))
                      " W" (number-to-string (+mode-line-count-words beg end))
                      " ")))
      +mode-line-region-cache-value)))

(defsubst +mode-line-overwrite-readonly-indicator ()
  "Display whether it is in overwrite mode or read-only buffer."
  (let ((ro (when buffer-read-only " %%"))
        (ov (when overwrite-mode " #")))
    (concat ro ov " ")))

(defun +mode-line-symbol-overlay-indicator ()
  "Display the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (when (symbol-overlay-assoc symbol)
        (concat "| " (number-to-string (1+ count))
                "/" (number-to-string (+ count (length after)))
                " sym "
                (and (cadr keyword) "in scope "))))))

;;; Cache remote host name
(defvar-local +mode-line-remote-host-name nil)
(defun +mode-line-update-remote-host-name ()
  "Hostname for remote buffers."
  (setq +mode-line-remote-host-name
        (when-let* ((hostname (and default-directory
                                   (file-remote-p default-directory 'host))))
          (when (not (string-equal hostname "localhost"))
            (concat "@" hostname)))))

;;; Cache encoding info
(defvar-local +mode-line-encoding nil)
(defun +mode-line-update-encoding (&rest _)
  "Get encoding and EOL type of current buffer."
  (setq +mode-line-encoding
        (unless (and (memq (coding-system-category buffer-file-coding-system)
                           '(coding-category-undecided coding-category-utf-8))
                     (eq (coding-system-eol-type buffer-file-coding-system) 0))
          "%Z")))


;;; [vcs-info] cache for vcs
(defvar-local +mode-line-vcs-info nil)
(defun +mode-line-update-vcs-info ()
  "Cache version-control information for the current buffer."
  (setq +mode-line-vcs-info
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend))
                 (rev     (if +mode-line-show-common-vc-tools-name
                              (substring-no-properties vc-mode 1)
                            (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
                 (face (cond ((eq state 'up-to-date) 'vc-dir-status-up-to-date)
                             ((eq state 'ignored) 'vc-dir-status-ignored)
                             ((memq state '(needs-update needs-merge conflict missing)) 'vc-dir-status-warning)
                             (t 'vc-dir-status-edited)))
                 (state-symbol (cond ((eq state 'up-to-date) "√")
                                     ((eq state 'edited) "*")
                                     ((eq state 'added) "@")
                                     ((eq state 'needs-update) "￬")
                                     ((eq state 'needs-merge) "&")
                                     ((eq state 'unlocked-changes) "")
                                     ((eq state 'removed) "×")
                                     ((eq state 'conflict) "!")
                                     ((eq state 'missing) "?")
                                     ((eq state 'ignored) "-")
                                     ((eq state 'unregistered) "+")
                                     ((stringp state) (concat "#" state ":"))
                                     (t " "))))
            (concat " "
                    (propertize (concat rev state-symbol)
                                'face face
                                'help-echo (get-text-property 1 'help-echo vc-mode)))))))


(defun +nerd-icons-icon-for-buffer ()
  "Return a Nerd Font icon for the current buffer."
  (interactive)
  (if (or (eq major-mode 'eaf-mode))
      ;; WORKAROUND: eaf don't have `buffer-file-name`
      (nerd-icons-icon-for-file (buffer-name))
    (nerd-icons-icon-for-buffer)))

(defvar-local +mode-line-icon-cache-key nil)
(defvar-local +mode-line-icon-cache-value nil)

(defun +mode-line-buffer-icon ()
  "Return cached nerd icon for the current buffer."
  (let ((key (list (buffer-name) buffer-file-name major-mode)))
    (unless (equal key +mode-line-icon-cache-key)
      (setq +mode-line-icon-cache-key key
            +mode-line-icon-cache-value (+nerd-icons-icon-for-buffer)))
    +mode-line-icon-cache-value))

(defun +mode-line-active-indicators ()
  "Return indicators only shown on the selected window."
  (concat (+mode-line-macro-indicator)
          (+mode-line-symbol-overlay-indicator)
          (+mode-line-use-region-indicator)))

(use-package flymake
  :ensure nil
  :defines flymake--state
  :preface
  (defun thy/mode-line-flymake-counters ()
    "Return Flymake counters separated by slashes when Flymake is ready."
    (when (and (bound-and-true-p flymake-mode)
               flymake--state
               (flymake-running-backends))
      (let (result)
        (dolist (type '(:error :warning :note))
          (let ((counter (flymake--mode-line-counter type)))
            (when counter
              (when result
                (setq result (append result '("/"))))
              ;; Each built-in counter starts with its own spacing element.
              (setq result (append result (cdr counter))))))
        (when result
          (cons " " result))))))

(defun +mode-line-left (active-p meta-face panel-face)
  "Return left mode-line for ACTIVE-P using META-FACE and PANEL-FACE."
  (let ((active-indicators (when active-p (+mode-line-active-indicators))))
    `((:propertize ,(+mode-line-get-window-name) face ,panel-face)
      (:propertize ,(+mode-line-overwrite-readonly-indicator) face ,panel-face)
      (,active-p (:propertize ,active-indicators face ,panel-face))
      " "
      ,(or +mode-line-project-crumb
           `(:propertize "%b" face ,meta-face))
      " "
      (:propertize +mode-line-remote-host-name face +mode-line-host-name-active-face))))

(defun +mode-line-right (active-p)
  "Return right mode-line for ACTIVE-P."
  (let ((icon (+mode-line-buffer-icon)))
    `((,active-p ,icon (:propertize ,icon face nil))
      "  "
      (:propertize mode-name face ,(when active-p '+mode-line-mode-name-active-face))
      (,active-p ,+mode-line-vcs-info (:propertize ,+mode-line-vcs-info face nil))
      (,active-p (:eval (thy/mode-line-flymake-counters)))
      " "
      +mode-line-encoding
      "%l:%p%%"
      " ")))

(defsubst +mode-line-compute ()
  "Formatting active-long mode-line."
  (let* ((meta-face (+mode-line-get-window-name-face))
         (active-p (mode-line-window-selected-p))
         (panel-face `(:inherit ,meta-face :inverse-video ,active-p))
         (lhs (+mode-line-left active-p meta-face panel-face))
         (rhs (+mode-line-right active-p))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(use-package emacs
  :ensure nil
  :init
  (dolist (hook '(after-revert-hook buffer-list-update-hook window-size-change-functions))
    (add-hook hook #'+mode-line-window-size-change-function))
  (add-hook 'find-file-hook #'+mode-line-update-remote-host-name)
  (setq-default mode-line-format
                '((:eval (+mode-line-compute)))
                header-line-format nil))

(use-package mule
  :ensure nil
  :init
  (setq eol-mnemonic-unix "LF"
        eol-mnemonic-dos "CRLF"
        eol-mnemonic-mac "CR"
        eol-mnemonic-undecided "?")
  (dolist (hook '(find-file-hook after-change-major-mode-hook))
    (add-hook hook #'+mode-line-update-encoding))
  (advice-add #'after-insert-file-set-coding :after #'+mode-line-update-encoding)
  (advice-add #'set-buffer-file-coding-system :after #'+mode-line-update-encoding))

(use-package vc
  :ensure nil
  :hook (after-save . +mode-line-update-vcs-info)
  :config
  (advice-add #'vc-refresh-state :after #'+mode-line-update-vcs-info))

;;; Breadcrumb project/imenu crumbs for the mode-line.
(use-package breadcrumb
  :ensure t
  :preface
  (defvar-local +mode-line-project-crumb nil)

  (defun +mode-line-update-project-crumb (&rest _)
    "Cache breadcrumb project crumbs for the mode-line."
    (setq +mode-line-project-crumb
          (when (fboundp 'breadcrumb-project-crumbs)
            (breadcrumb-project-crumbs))))
  :init
  (dolist (hook '(find-file-hook after-save-hook clone-indirect-buffer-hook
                                 Info-selection-hook window-configuration-change-hook))
    (add-hook hook #'+mode-line-update-project-crumb))
  (dolist (fn '(rename-buffer set-visited-file-name pop-to-buffer popup-create popup-delete))
    (advice-add fn :after #'+mode-line-update-project-crumb))
  :custom-face
  (breadcrumb-project-base-face ((t (:inherit breadcrumb-project-crumbs-face :bold t :slant italic))))
  (breadcrumb-project-crumbs-face ((t (:slant italic))))
  (breadcrumb-project-leaf-face ((t (:inherit font-lock-function-name-face :bold t :slant italic))))
  (breadcrumb-imenu-leaf-face ((t (:inherit font-lock-function-name-face :foreground unspecified))))
  :config
  (setq breadcrumb-imenu-crumb-separator " ⋅ "
        breadcrumb-project-max-length 0.55
        breadcrumb-idle-time 10))
