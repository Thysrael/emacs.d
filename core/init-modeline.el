;;; -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

;; show encodings for UTF-8:LF
(defvar +mode-line-show-common-encodings nil)
;; show VC tools name for Git
(defvar +mode-line-show-common-vc-tools-name nil)

;;; Check whether `window-total-width' is larger than the limit
(defconst +mode-line-window-width-limit 90)
(defvar-local +mode-line-enough-width-p nil)
(defun +mode-line-window-size-change-function (&rest _)
  (setq +mode-line-enough-width-p
        (> (window-total-width) +mode-line-window-width-limit)))
(dolist (hook '(after-revert-hook buffer-list-update-hook window-size-change-functions))
  (add-hook hook #'+mode-line-window-size-change-function))

;;; face
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

(defsubst +mode-line-use-region-indicator ()
  "Display selected region in current buffer."
  (when (use-region-p)
    (concat "| L" (number-to-string (count-lines (region-beginning) (region-end)))
            " C" (number-to-string (abs (- (mark t) (point))))
            " W"  (number-to-string (count-words (region-beginning) (region-end))) " ")))

(defsubst +mode-line-overwrite-readonly-indicator ()
  "Display whether it is in overwrite mode or read-only buffer."
  (let ((ro (when buffer-read-only " %%"))
        (ov (when overwrite-mode " #")))
    (concat ro ov " ")))

(defsubst +mode-line-symbol-overlay-indicator ()
  "Display the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol)))
    (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
           (symbol (car keyword))
           (before (symbol-overlay-get-list -1 symbol))
           (after (symbol-overlay-get-list 1 symbol))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (concat  "| " (number-to-string (1+ count))
                   "/" (number-to-string (+ count (length after)))
                   " sym "
                   (and (cadr keyword) "in scope "))))))

;;; [project-crumb] Cache project path info.
(defvar-local +mode-line-project-crumb nil)

(defun +mode-line-update-project-crumb (&rest _)
  "Cache breadcrumb project crumbs for the mode-line."
  (setq +mode-line-project-crumb
        (when (fboundp 'breadcrumb-project-crumbs)
          (breadcrumb-project-crumbs))))

(dolist (hook '(find-file-hook after-save-hook clone-indirect-buffer-hook
                               Info-selection-hook window-configuration-change-hook))
  (add-hook hook #'+mode-line-update-project-crumb))

(dolist (fn '(rename-buffer set-visited-file-name pop-to-buffer popup-create popup-delete))
  (advice-add fn :after #'+mode-line-update-project-crumb))


;;; Cache remote host name
(defvar-local +mode-line-remote-host-name nil)
(defun +mode-line-update-remote-host-name ()
  "Hostname for remote buffers."
  (setq +mode-line-remote-host-name
        (when-let ((hostname (and default-directory
                                  (file-remote-p default-directory 'host))))
          (when (not (string-equal hostname "localhost"))
            (concat "@" hostname)))))
(add-hook 'find-file-hook #'+mode-line-update-remote-host-name)

;;; Cache flymake report
(defvar-local +mode-line-flymake-indicator nil)
(defun +mode-line-update-flymake (&rest _)
  "Display flymake info for current buffer."
  (setq +mode-line-flymake-indicator
        (when (and flymake-mode (flymake-running-backends))
          (let* ((err-count (cadadr (flymake--mode-line-counter :error)))
                 (warning-count (cadadr (flymake--mode-line-counter :warning)))
                 (note-count (cadadr (flymake--mode-line-counter :note)))
                 (err (when (and err-count (not (string= err-count "0")))
                        (propertize err-count 'face '(:inherit compilation-error))))
                 (warning (when (and warning-count (not (string= warning-count "0")))
                            (propertize warning-count 'face '(:inherit compilation-warning))))
                 (note (when (and note-count (not (string= note-count "0")))
                         (propertize note-count 'face '(:inherit compilation-info))))
                 (info (string-join (remove nil (list err warning note)) "/")))
            (when (not (string-empty-p info)) (concat " " info))))))
(add-hook 'flymake-mode-hook #'+mode-line-update-flymake)
(with-eval-after-load 'flymake
  (advice-add #'flymake--handle-report :after #'+mode-line-update-flymake))

;;; Cache encoding info
(setq eol-mnemonic-unix "LF"
      eol-mnemonic-dos "CRLF"
      eol-mnemonic-mac "CR"
      eol-mnemonic-undecided "?")

(defvar-local +mode-line-encoding nil)
(defun +mode-line-update-encoding (&rest _)
  "Get encoding and EOL type of current buffer."
  (setq +mode-line-encoding
        (unless (and (memq (coding-system-category buffer-file-coding-system)
                           '(coding-category-undecided coding-category-utf-8))
                     (eq (coding-system-eol-type buffer-file-coding-system) 0))
          "%Z")))
(add-hook 'find-file-hook #'+mode-line-update-encoding)
(advice-add #'after-insert-file-set-coding :after #'+mode-line-update-encoding)
(advice-add #'set-buffer-file-coding-system :after #'+mode-line-update-encoding)


;;; [vcs-info] cache for vcs
(defvar-local +mode-line-vcs-info nil)
(defun +mode-line-update-vcs-info ()
  (when (and vc-mode buffer-file-name)
    (setq +mode-line-vcs-info
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
(dolist (hook '(find-file-hook after-save-hook))
  (add-hook hook #'+mode-line-update-vcs-info))
(with-eval-after-load 'vc
  (advice-add #'vc-refresh-state :after #'+mode-line-update-vcs-info))


(defun +nerd-icons-icon-for-buffer ()
  (interactive)
  (if (or (eq major-mode 'eaf-mode))
      ;; WORKAROUND: eaf don't have `buffer-file-name`
      (nerd-icons-icon-for-file (buffer-name))
    (nerd-icons-icon-for-buffer)))

;; [buffer position]
(defsubst +mode-line-buffer-position ()
  (let ((pos (format-mode-line '(-3 "%p"))))
    (pcase pos
      ("Top" "⊤")
      ("Bot" "⊥")
      ("All" "∀")
      (_ "%p%%"))))

(defsubst +mode-line-compute ()
  "Formatting active-long mode-line."
  (let* ((meta-face (+mode-line-get-window-name-face))
         (active-p (mode-line-window-selected-p))
         (panel-face `(:inherit ,meta-face :inverse-video ,active-p))
         (lhs `(
         (:propertize ,(+mode-line-get-window-name)
                             face ,panel-face)
                (:propertize ,(+mode-line-overwrite-readonly-indicator)
                             face ,panel-face)
                (,active-p (:propertize
                            ,(concat (+mode-line-macro-indicator)
                                     (+mode-line-symbol-overlay-indicator)
                                     (+mode-line-use-region-indicator))
                            face ,panel-face))
                " "
               ,(or +mode-line-project-crumb
                     `(:propertize "%b" face ,meta-face))
                " "
                (:propertize +mode-line-remote-host-name
                          face +mode-line-host-name-active-face)
                ))
         (rhs `(
                (,active-p ,(+nerd-icons-icon-for-buffer) ; 选中时使用彩色 icon
                         (:propertize ,(+nerd-icons-icon-for-buffer) face nil)); 非选中的时候选用无色 icon
                "  "
                (:propertize mode-name face ,(when active-p '+mode-line-mode-name-active-face))
                (,active-p ,+mode-line-vcs-info
                           (:propertize ,+mode-line-vcs-info face nil))
                (,active-p ,+mode-line-flymake-indicator)
                " "
                (:eval +mode-line-encoding)
                "%l:"
                ;; (:eval (+mode-line-buffer-position))
                " "
                ))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    `(,lhs
      ,(propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,rhs-w))))
      ,rhs-str)))

(setq-default mode-line-format
               '((:eval (+mode-line-compute)))
               header-line-format nil)

;;; Breadcrumb project/imenu crumbs for the mode-line.
(use-package breadcrumb
  :ensure t
  :custom-face
  (breadcrumb-project-base-face ((t (:inherit breadcrumb-project-crumbs-face :bold t))))
  (breadcrumb-project-leaf-face ((t (:inherit font-lock-function-name-face :bold t))))
  (breadcrumb-imenu-leaf-face ((t (:inherit font-lock-function-name-face :foreground unspecified))))
  :config
  (setq breadcrumb-imenu-crumb-separator " ⋅ "
        breadcrumb-project-max-length 0.55
        breadcrumb-idle-time 10))
