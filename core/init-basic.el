;;; -*- lexical-binding: t -*-

;; Basic Emacs behavior, editing defaults, history, and runtime tuning.

(use-package emacs
  :ensure nil
  :demand t
  :preface
  (defun thy/kill-region-or-line ()
    "Kill the active region, or the current line when no region is active."
    (interactive)
    (if (use-region-p)
        (let ((start (region-beginning)))
          (kill-region start (region-end))
          (goto-char start))
      (kill-whole-line)
      (forward-line -1)
      (end-of-line)))

  (defun thy/copy-region-or-line ()
    "Save the active region, or the current line, to the kill ring."
    (interactive)
    (let ((start (point)))
      (if (use-region-p)
          (kill-ring-save (region-beginning) (region-end))
        (kill-ring-save (line-beginning-position) (line-end-position)))
      (goto-char start)))

  (defun thy/after-find-file-without-sit-for (fn &rest args)
    "Call FN with ARGS while suppressing `sit-for' delays."
    (cl-letf (((symbol-function #'sit-for) #'ignore))
      (apply fn args)))
  :bind
  (("C-q" . thy/kill-region-or-line)
   ("C-w" . thy/copy-region-or-line)
   ("C-S-v" . yank)
   ("C-S-x" . thy/kill-region-or-line)
   ("C-S-c" . thy/copy-region-or-line)
   ("<f5>" . revert-buffer)
   ("<f2>" . kmacro-set-counter)
   ("C-z" . undo))
  :hook
  ;; Let typed text replace the active region.
  (after-init . delete-selection-mode)
  ;; Wrap long lines visually in prose buffers.
  (text-mode . visual-line-mode)
  :custom
  ;; Use autosave files so recovery remains possible after crashes.
  (auto-save-default t)
  ;; Kill subprocesses directly when quitting Emacs.
  (confirm-kill-processes nil)
  ;; Do not prompt when visiting a new file path.
  (confirm-nonexistent-file-or-buffer nil)
  ;; Do not create .# lock files next to visited files.
  (create-lockfiles nil)
  (file-name-shadow-mode t)
  ;; Avoid noisy warnings when the same file is opened via another path.
  (find-file-suppress-same-file-warnings t)
  ;; Follow symlinks to the real file path.
  (find-file-visit-truename t)
  ;; Avoid duplicate kill-ring entries.
  (kill-do-not-save-duplicates t)
  ;; Do not create file~ backup files next to visited files.
  (make-backup-files nil)
  ;; Make completion ignore case for buffer and file names.
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; Ensure saved files end with a final newline.
  (require-final-newline t)
  ;; Disable the audible and visible bell.
  (ring-bell-function #'ignore)
  ;; Keep replaced external clipboard text in the kill ring.
  (save-interprogram-paste-before-kill t)
  ;; Share the clipboard with terminal Emacs and external programs.
  (select-enable-clipboard t)
  ;; Treat Chinese punctuation as sentence endings.
  (sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (sentence-end-double-space nil)
  ;; Make Tab indent the current line or active region.
  (tab-always-indent t)
  ;; Disambiguate buffers with the same file name using path components.
  (uniquify-buffer-name-style 'forward)
  ;; Prefer y-or-n prompts over yes-or-no prompts.
  (use-short-answers t)
  (vc-follow-symlinks t)
  :config
  ;; Preserve large deletions in autosave data.
  (setq auto-save-include-big-deletions t)

  ;; Reject remote host probing in file-at-point helpers.
  (setq ffap-machine-p-known 'reject)

  ;; Use a larger process read chunk for LSPs and other external tools.
  (setq read-process-output-max (* 3 1024 1024))

  ;; Hide server client instructions in client frames.
  (setq server-client-instructions nil)

  ;; `tabify' should only convert indentation spaces.
  (setq tabify-regexp "^\t* [ \t]+")

  ;; Use a simple ASCII ellipsis for truncated text.
  (setq truncate-string-ellipsis "...")

  ;; Keep autosaves under the no-littering autosave directory, with a TRAMP-safe
  ;; name so remote and local autosave files do not conflict.
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           ,(concat auto-save-list-file-prefix "tramp-\\2") t)
          (".*" ,auto-save-list-file-prefix t)))

  ;; Prefer left-to-right layout and skip bidirectional text scanning for speed.
  (setq-default bidi-display-reordering t)
  (setq-default bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Default editing behavior for new buffers.
  (setq-default fill-column 100)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; Default long-line display behavior for new buffers.
  (setq-default truncate-lines t)
  (setq-default truncate-partial-width-windows nil)
  (setq-default word-wrap t)
  (setq-default word-wrap-by-category t)

  (set-language-environment "UTF-8")

  ;; Put the command prefix on the right-hand home row.
  (define-key key-translation-map (kbd "C-j") (kbd "C-x"))

  ;; Translate modified Chinese punctuation to ASCII punctuation.
  (cl-loop for prefix in '("C-" "M-" "s-" "H-")
           do (cl-loop for cpunc in '("，" "。" "？" "！" "；" "：" "、"
                                      "（" "）" "【" "】" "《" "》" "—")
                       for epunc in '("," "." "?" "!" ";" ":" ","
                                      "(" ")" "[" "]" "<" ">" "_")
                       do (define-key key-translation-map
                            (kbd (concat prefix cpunc))
                            (kbd (concat prefix epunc)))))

  (when (eq system-type 'darwin)
    ;; Use Command as Meta in macOS GUI frames.
    (setq ns-command-modifier 'meta)
    (setq ns-option-modifier 'alt))

  ;; Speed up auto-save recovery prompts by avoiding `sit-for' delays.
  (advice-add #'after-find-file :around #'thy/after-find-file-without-sit-for))
;; Improve behavior for files that contain extremely long lines.
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :config
  ;; Do not persist cursor positions for very long files.
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil)))

;; Remember the last visited position in files across Emacs sessions.
(use-package saveplace
  :ensure nil
  :preface
  (defun thy/save-place-alist-to-file-with-prin1 (fn)
    "Call FN while using `prin1' instead of `pp' for save-place state."
    (cl-letf (((symbol-function #'pp) #'prin1))
      (funcall fn)))
  :hook (after-init . save-place-mode)
  :config
  ;; `save-place-alist-to-file' pretty-prints its cache with `pp', which is
  ;; expensive and unnecessary for machine-written state.
  (advice-add #'save-place-alist-to-file :around #'thy/save-place-alist-to-file-with-prin1))

;; Open recently visited files with C-x C-r.
(use-package recentf
  :ensure nil
  :preface
  (defun thy/recentf-add-dired-directory ()
    "Add the current Dired directory to `recentf'."
    (recentf-add-file default-directory))
  :bind ("C-x C-r" . recentf-open-files)
  :hook
  (after-init . recentf-mode)
  (dired-mode . thy/recentf-add-dired-directory)
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-keep nil)
  (recentf-max-saved-items 200)
  :config
  ;; Keep generated state out of the recent file list.
  (add-to-list 'recentf-exclude (recentf-expand-file-name no-littering-var-directory))

  ;; Store shorter, property-free file names.
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;; Save minibuffer, command, search, mark, and kill-ring history across sessions.
(use-package savehist
  :ensure nil
  :preface
  (defun thy/savehist-strip-text-properties ()
    "Remove text properties from saved kill-ring and register strings."
    (setq kill-ring
          (mapcar #'substring-no-properties
                  (cl-remove-if-not #'stringp kill-ring)))
    (setq register-alist
          (cl-loop for (reg . item) in register-alist
                   if (stringp item)
                   collect (cons reg (substring-no-properties item))
                   else collect (cons reg item))))
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   winner-ring
                                   kill-ring))
  (savehist-autosave-interval 300)
  :config
  ;; Reduce savehist cache size by dropping text properties from large saved values.
  (add-hook 'savehist-save-hook #'thy/savehist-strip-text-properties)
  (with-eval-after-load 'vertico
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

(use-package gcmh
  :ensure t
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold #x64000000)
  (gcmh-idle-delay 'auto))
