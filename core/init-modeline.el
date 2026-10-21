;;; -*- lexical-binding: t -*-

(use-package emacs
  :ensure nil
  :preface
  (defgroup thy/mode-line nil
    "Personal mode-line faces."
    :group 'mode-line)

  (defface thy/mode-line-unchanged
    '((t (:inherit (font-lock-function-name-face bold))))
    "Face for an unmodified buffer's status."
    :group 'thy/mode-line)

  (defface thy/mode-line-modified
    '((t (:inherit (font-lock-keyword-face bold))))
    "Face for a modified buffer's status."
    :group 'thy/mode-line)

  (defface thy/mode-line-major-mode
    '((t (:inherit font-lock-function-name-face)))
    "Face for the selected window's major mode."
    :group 'thy/mode-line)

  (defface thy/mode-line-host
    '((t (:weight bold :slant italic)))
    "Face for a remote host name."
    :group 'thy/mode-line)

  (defvar-local thy/mode-line-region-key nil
    "Selection bounds and modification tick used for cached statistics.")

  (defvar-local thy/mode-line-region nil
    "Cached line, character, and word counts for the active region.")

  (defun thy/mode-line-buffer-face ()
    "Return the status face for the current buffer."
    (if (buffer-modified-p) 'thy/mode-line-modified 'thy/mode-line-unchanged))

  (defun thy/mode-line-update-region ()
    "Update selection statistics after a command, outside redisplay."
    (if (not (use-region-p))
        (setq thy/mode-line-region-key nil
              thy/mode-line-region nil)
      (let* ((beg (region-beginning))
             (end (region-end))
             (emt-loaded (bound-and-true-p emt--lib-loaded))
             (key (list beg end (buffer-chars-modified-tick) emt-loaded)))
        (unless (equal key thy/mode-line-region-key)
          (let ((words
                 (or (when (and emt-loaded (fboundp 'emt-split))
                       (ignore-errors
                         (length (emt-split (buffer-substring-no-properties beg end)))))
                     (count-words beg end))))
            (setq thy/mode-line-region-key key
                  thy/mode-line-region
                  (format "| L%d C%d W%d " (count-lines beg end) (- end beg) words)))))))

  (defun thy/mode-line-symbol-indicator ()
    "Return the current symbol's match index and total in one overlay pass."
    (when (and (bound-and-true-p symbol-overlay-keywords-alist)
               (not (bound-and-true-p symbol-overlay-temp-symbol)))
      (when-let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t))))
        (let ((before 0)
              (total 0))
          (dolist (overlay (symbol-overlay-get-list 0 (car keyword)))
            (setq total (1+ total))
            (when (< (overlay-end overlay) (point))
              (setq before (1+ before))))
          (when (> total 0)
            (format "| %d/%d sym %s" (1+ before) total
                    (if (cadr keyword) "in scope " "")))))))

  (defun thy/mode-line-status ()
    "Return the window label and editing status for the mode line."
    (let* ((active (mode-line-window-selected-p))
           (text (concat " " (window-parameter (selected-window) 'ace-window-path)
                         (when buffer-read-only " %")
                         (when overwrite-mode " #")
                         " "
                         (when active
                           (concat (cond (defining-kbd-macro "| MacroDef ")
                                         (executing-kbd-macro "| MacroExc "))
                                   (thy/mode-line-symbol-indicator)
                                   (when (use-region-p) thy/mode-line-region))))))
      (propertize (string-replace "%" "%%" text)
                  'face `(:inherit ,(thy/mode-line-buffer-face)
                          :inverse-video ,active))))

  (defvar-local thy/mode-line-host-key nil
    "Directory used for the cached remote host name.")

  (defvar-local thy/mode-line-host-name nil
    "Cached remote host name for the current directory.")

  (defun thy/mode-line-remote-host ()
    "Return a cached host name without contacting the remote machine."
    (unless (equal default-directory thy/mode-line-host-key)
      (setq thy/mode-line-host-key default-directory
            thy/mode-line-host-name
            (when-let* ((host (file-remote-p default-directory 'host))
                        ((not (equal host "localhost"))))
              (propertize (concat " @" (string-replace "%" "%%" host))
                          'face 'thy/mode-line-host))))
    thy/mode-line-host-name)
  :hook
  ((post-command . thy/mode-line-update-region)
   (deactivate-mark . thy/mode-line-update-region))
  :custom
  (mode-line-right-align-edge 'window)
  (mode-line-format
   '("%e"
     (:eval (thy/mode-line-status))
     " "
     (:eval (thy/mode-line-project-crumb))
     (:eval (thy/mode-line-remote-host))
     mode-line-format-right-align
     (:eval (thy/mode-line-icon))
     "  "
     (:eval (if (mode-line-window-selected-p)
                '(:propertize mode-name face thy/mode-line-major-mode)
              'mode-name))
     (:eval (if (mode-line-window-selected-p)
                'vc-mode
              '(:propertize vc-mode face nil)))
     (:eval (when (mode-line-window-selected-p)
              (thy/mode-line-flymake-counters)))
     " "
     thy/mode-line-encoding
     "%l:%p "))
  :init
  (setq-default header-line-format nil))

(use-package nerd-icons
  :ensure t
  ;; Mode-line providers must load before redisplay evaluates their functions.
  :demand t
  :preface
  (defvar-local thy/mode-line-icon-key nil
    "Buffer identity used for the cached mode-line icon.")

  (defvar-local thy/mode-line-icon-pair nil
    "Cached active and inactive mode-line icons.")

  (defun thy/mode-line-icon ()
    "Return the cached icon for the window being displayed."
    (let ((key (list (buffer-name) buffer-file-name major-mode)))
      (unless (equal key thy/mode-line-icon-key)
        (let ((icon (nerd-icons-icon-for-buffer)))
          (setq thy/mode-line-icon-key key
                thy/mode-line-icon-pair (cons icon (propertize icon 'face nil)))))
      (if (mode-line-window-selected-p)
          (car thy/mode-line-icon-pair)
        (cdr thy/mode-line-icon-pair)))))

(use-package mule
  :ensure nil
  :preface
  (defvar-local thy/mode-line-encoding nil
    "Mode-line construct for non-default coding systems and line endings.")

  (defun thy/mode-line-update-encoding (&rest _)
    "Show encoding unless the buffer uses UTF-8 or undecided with LF."
    (setq thy/mode-line-encoding
          (unless (and (memq (coding-system-category buffer-file-coding-system)
                             '(coding-category-undecided coding-category-utf-8))
                       (eq (coding-system-eol-type buffer-file-coding-system) 0))
            ;; A list expands %Z; a string-valued symbol displays it literally.
            '("%Z "))))
  :hook
  ((find-file after-change-major-mode) . thy/mode-line-update-encoding)
  :custom
  (eol-mnemonic-unix "LF")
  (eol-mnemonic-dos "CRLF")
  (eol-mnemonic-mac "CR")
  (eol-mnemonic-undecided "?")
  :init
  (advice-add #'after-insert-file-set-coding :after #'thy/mode-line-update-encoding)
  (advice-add #'set-buffer-file-coding-system :after #'thy/mode-line-update-encoding))

(use-package vc-hooks
  :ensure nil
  :custom
  ;; Reuse VC's cached status, tooltip, and mouse menu.
  (vc-display-status 'no-backend))

(use-package vc-git
  :ensure nil
  :preface
  (defun thy/mode-line-git-status (function file)
    "Format FUNCTION's Git status for FILE with a suffix and VC directory face."
    (let ((text (funcall function file)))
      (if (not (eq vc-display-status 'no-backend))
          text
        (let* ((state (vc-file-getprop file 'vc-state))
               (face (pcase state
                       ('up-to-date 'vc-dir-status-up-to-date)
                       ('ignored 'vc-dir-status-ignored)
                       ((or 'needs-update 'needs-merge 'conflict 'missing)
                        'vc-dir-status-warning)
                       (_ 'vc-dir-status-edited)))
               (suffix (pcase state
                         ('up-to-date "")
                         ('edited "*")
                         ('added "+")
                         ('conflict "!")
                         ('removed "×")
                         ('missing "?")
                         (_ (substring text 0 1)))))
          (propertize
           (concat (substring text 1)
                   (apply #'propertize suffix (text-properties-at 0 text)))
           'face face)))))
  :config
  (require 'vc-dir)
  ;; Format only when VC refreshes its cache, outside mode-line redisplay.
  (advice-add #'vc-git-mode-line-string :around #'thy/mode-line-git-status))

(use-package flymake
  :ensure nil
  :defines flymake--state
  :preface
  (defun thy/mode-line-flymake-counters ()
    "Return cached Flymake counters separated by slashes when ready."
    (when (and (bound-and-true-p flymake-mode)
               flymake--state
               (flymake-running-backends))
      (let (result)
        (dolist (type '(:error :warning :note))
          (when-let* ((counter (flymake--mode-line-counter type)))
            (when result (push "/" result))
            ;; Each built-in counter starts with its own spacing element.
            (dolist (part (cdr counter)) (push part result))))
        (when result (cons " " (nreverse result)))))))

(use-package breadcrumb
  :ensure t
  :demand t
  :preface
  (defvar-local thy/mode-line-project-crumb-cache nil
    "Project crumbs cached separately for each window's width.")

  (defun thy/mode-line-invalidate-project (&rest _)
    "Invalidate project crumbs in all windows showing the current buffer."
    (setq thy/mode-line-project-crumb-cache nil)
    (force-mode-line-update))

  (defun thy/mode-line-project-crumb ()
    "Return project crumbs bounded by the current window's width."
    (unless thy/mode-line-project-crumb-cache
      (setq thy/mode-line-project-crumb-cache
            (make-hash-table :test #'eq :weakness 'key))
      (add-hook 'window-buffer-change-functions
                #'thy/mode-line-invalidate-project nil t))
    (let* ((window (selected-window))
           (buffer-file-name (or (bound-and-true-p thy/office-preview-source-file)
                                 buffer-file-name))
           (width (max 1 (if (floatp breadcrumb-project-max-length)
                            (floor (* (window-total-width window)
                                      breadcrumb-project-max-length))
                          breadcrumb-project-max-length)))
           (key (list (buffer-name) buffer-file-name default-directory width))
           (cached (gethash window thy/mode-line-project-crumb-cache)))
      (unless (equal key (car cached))
        (let* ((non-essential t)
               (text (truncate-string-to-width (breadcrumb-project-crumbs)
                                               width nil nil t)))
          ;; File names containing percent signs must not become format codes.
          (setq cached (cons key (string-replace "%" "%%" text)))
          (puthash window cached thy/mode-line-project-crumb-cache)))
      (cdr cached)))
  :hook
  ((find-file after-save clone-indirect-buffer Info-selection) . thy/mode-line-invalidate-project)
  :init
  (dolist (function '(rename-buffer set-visited-file-name))
    (advice-add function :after #'thy/mode-line-invalidate-project))
  :custom
  (breadcrumb-imenu-crumb-separator " ⋅ ")
  (breadcrumb-project-max-length 0.4)
  :custom-face
  (breadcrumb-project-base-face
   ((t (:inherit breadcrumb-project-crumbs-face :bold t :slant italic))))
  (breadcrumb-project-crumbs-face
   ((t (:inherit font-lock-function-name-face :slant italic))))
  (breadcrumb-project-leaf-face
   ((t (:inherit font-lock-function-name-face :bold t :slant italic))))
  (breadcrumb-imenu-leaf-face
   ((t (:inherit font-lock-function-name-face :foreground unspecified))))
  :config
  (setq breadcrumb-idle-time 10))
