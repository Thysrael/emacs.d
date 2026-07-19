;;; -*- lexical-binding: t -*-

(use-package vc
  :ensure nil
  :custom
  (vc-handled-backends '(Git)))

(use-package vc-git
  :ensure nil
  :custom
  (vc-git-diff-switches '("--histogram")))

(use-package diff-hl
  :ensure t
  :defines desktop-minor-mode-table
  :preface
  (defun thy/consult-diff-hunks ()
    "Browse the current buffer's changed hunks with live preview."
    (interactive)
    (require 'consult)
    (require 'diff-hl)
    (unless (and buffer-file-name (vc-backend buffer-file-name))
      (user-error "The buffer is not under version control"))
    (let ((buffer (current-buffer))
          candidates)
      (save-restriction
        (widen)
        (diff-hl-update)
        (let ((hunks
               (sort (seq-filter
                      (lambda (overlay) (overlay-get overlay 'diff-hl-hunk))
                      (overlays-in (point-min) (point-max)))
                     (lambda (left right)
                       (< (overlay-start left) (overlay-start right))))))
          (dolist (overlay hunks)
            (let* ((start (overlay-start overlay))
                   (end (overlay-end overlay))
                   (line (line-number-at-pos start))
                   (type (overlay-get overlay 'diff-hl-hunk-type))
                   (summary
                    (string-trim
                     (replace-regexp-in-string
                      "[[:space:]\n]+" " "
                      (buffer-substring-no-properties start end))))
                   (label
                    (propertize
                     (pcase type
                       ('change "edit")
                       ('insert "add")
                       ('delete "delete")
                       (_ (symbol-name type)))
                     'face (intern (format "diff-hl-%s" type))))
                   (candidate
                    (format "%-6s %5d  %s"
                            label line
                            (if (string-empty-p summary)
                                "(empty line)"
                              (truncate-string-to-width
                               summary 100 nil nil "...")))))
              (push (consult--location-candidate
                     candidate (cons buffer start) line line)
                    candidates)))))
      (setq candidates (nreverse candidates))
      (unless candidates
        (user-error "No changed hunks"))
      (consult--read candidates
                     :prompt "Diff hunk: "
                     :category 'consult-location
                     :sort nil
                     :require-match t
                     :lookup #'consult--lookup-location
                     :state (consult--location-state candidates))))

  (transient-define-prefix thy/diff-hunk-transient ()
    "Transient for navigating and acting on diff hunks."
    [["Navigate"
      ("n" "Next" diff-hl-next-hunk :transient t)
      ("p" "Previous" diff-hl-previous-hunk :transient t)
      ("s" "Show" diff-hl-show-hunk)]
     ["Act"
      ("r" "Revert" diff-hl-revert-hunk)
      ("S" "Stage" diff-hl-stage-current-hunk)]])
  :hook
  ((find-file . diff-hl-mode)
   (vc-dir-mode . diff-hl-dir-mode)
   (dired-mode . diff-hl-dired-mode)
   (focus-in . diff-hl-update-once)
   ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) .
    (lambda ()
      (unless (display-graphic-p) (diff-hl-margin-local-mode 1)))))
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-draw-borders nil)
  :bind
  (("C-c g" . thy/consult-diff-hunks)
   ("C-c G" . thy/diff-hunk-transient))
  :config
  (setq-default fringes-outside-margins t)

  (custom-theme-set-faces
   'user
   '(diff-hl-change ((t (:background "#82aaff" :foreground "#82aaff"))))
   '(diff-hl-delete ((t (:background "#ff757f" :foreground "#ff757f"))))
   '(diff-hl-insert ((t (:background "#77e0c6" :foreground "#77e0c6")))))

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))
(use-package magit
  :ensure t
  :bind (("C-c v" . magit)
         ("C-c V" . magit-blame-addition))
  :hook ((magit-process-mode . goto-address-mode))
  :custom
  (magit-bury-buffer-function #'thy/magit-kill-buffers)
  (magit-diff-paint-whitespace nil)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-revision-insert-related-refs nil)
  (magit-save-repository-buffers nil)
  :config
  (defun thy/magit-kill-buffers (&rest _)
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (magit-restore-window-configuration)
    (let ((buffers (magit-mode-get-buffers)))
      (when (eq major-mode 'magit-status-mode)
        (mapc (lambda (buf)
                (with-current-buffer buf
                  (if (and magit-this-process
                           (eq (process-status magit-this-process) 'run))
                      (bury-buffer buf)
                    (kill-buffer buf))))
              buffers)))))

(use-package smerge-mode
  :ensure nil
  :hook ((find-file . thy/smerge-try-smerge))
  :config
  (defun thy/smerge-try-smerge ()
    "Enable `smerge-mode' when the current file contains conflict markers."
    (when buffer-file-name
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (require 'smerge-mode)
          (smerge-mode 1))))))
