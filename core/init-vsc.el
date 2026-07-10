;;; -*- lexical-binding: t -*-

(use-package diff-hl
  :ensure t
  :defines desktop-minor-mode-table
  :hook
  (find-file    . diff-hl-mode)
  (vc-dir-mode  . diff-hl-dir-mode)
  (dired-mode   . diff-hl-dired-mode)
  ((focus-in . diff-hl-update-once))
  :hook
  ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) .
         (lambda ()
           (diff-hl-update-once)
           (unless (display-graphic-p) (diff-hl-margin-local-mode 1))))
  :custom
  (diff-hl-disable-on-remote t)
  (diff-hl-draw-borders nil)
  (vc-git-diff-switches '("--histogram"))
  :bind
  ("C-c g" . diff-hl-show-hunk)
  :custom-face
  (diff-hl-change ((t (:background "#82aaff" :foreground "#82aaff"))))
  (diff-hl-delete ((t (:background "#ff757f" :foreground "#ff757f"))))
  (diff-hl-insert ((t (:background "#77e0c6" :foreground "#77e0c6"))))
  :config
  (setq-default fringes-outside-margins t)

  (setq diff-hl-fringe-bmp-function
        (lambda (&rest _)
          (define-fringe-bitmap 'thy/diff-hl-bmp
            (vector #b00000000)
            1 8
            '(center t))))

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (with-eval-after-load 'flymake
    (setq flymake-fringe-indicator-position 'right-fringe))

  (advice-add #'ws-butler-after-save :after #'diff-hl-update-once))

(use-package magit
  :ensure t
  :bind (("C-c v" . magit)
         ("C-c V" . magit-blame-addition))
  :hook ((magit-process-mode . goto-address-mode))
  :custom
  (magit-bury-buffer-function #'thy/magit-kill-buffers)
  (magit-diff-paint-whitespace nil)
  (magit-diff-refine-hunk nil)
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
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (require 'smerge-mode)
          (smerge-mode 1))))))

(use-package blamer
  :ensure t
  :bind (("C-c G" . global-blamer-mode))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 40)
  (blamer-commit-formatter "* %s"))
