;;; -*- lexical-binding: t -*-

;; git chrunk
(use-package diff-hl
  :defines desktop-minor-mode-table
  :hook
  ; 在不同模式下挂在不同的高亮
  (find-file    . diff-hl-mode)
  (vc-dir-mode  . diff-hl-dir-mode)
  (dired-mode   . diff-hl-dired-mode)
  ((focus-in . diff-hl-update-once))
  ; Fall back to the display margin since the fringe is unavailable in tty
  :hook
  ((diff-hl-mode diff-hl-dir-mode diff-hl-dired-mode) .
         (lambda ()
           (diff-hl-update-once)
           (unless (display-graphic-p) (diff-hl-margin-local-mode 1))))
  :config
  (setq
   diff-hl-draw-borders nil
   ;; Reduce load on remote
   diff-hl-disable-on-remote t
   ;; A slightly faster algorithm for diffing
   vc-git-diff-switches '("--histogram"))

  ;; The gutter looks less cramped with some space between it and  buffer.
  (setq-default fringes-outside-margins t)

  ;; Make fringes look better
  (setq diff-hl-fringe-bmp-function
        (lambda (&rest _)
          (define-fringe-bitmap 'my-diff-hl-bmp
            (vector #b00000000)
            1 8
            '(center t))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Integration with flymake
  (with-eval-after-load 'flymake
    (setq flymake-fringe-indicator-position 'right-fringe))

  ;; WORKAROUND: Integration with ws-butler
  (advice-add #'ws-butler-after-save :after #'diff-hl-update-once)
  :bind
  ("C-c g" . diff-hl-show-hunk)
  )

;; [magit] Version control interface
(use-package magit
  :straight t
  :bind (("C-c v" . magit))
  :hook ((magit-process-mode . goto-address-mode))
  :config
  (setq
   ;; word-granularity diff
   magit-diff-refine-hunk t
   ;; dont paint whitespace
   magit-diff-paint-whitespace nil
   ;; Don't autosave repo buffers. This is too magical
   magit-save-repository-buffers nil
   ;; Don't display parent/related refs in commit buffers; they are rarely helpful and only add to runtime costs.
   magit-revision-insert-related-refs nil)

  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1) ; magit 全屏
  ;; Exterminate Magit buffers
  (defun +magit-kill-buffers (&rest _)
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
              buffers))))
  (setq magit-bury-buffer-function #'+magit-kill-buffers)
  )

(use-package forge
  :straight t
  :after magit
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :config
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Title" 60 t nil title  nil)
          ("State" 6 t nil state nil)
          ("Updated" 10 t nil updated nil)))
  )

;; Show TODOs in magit
(use-package magit-todos
  :straight t
  :after magit
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (with-eval-after-load 'magit-status
    (transient-append-suffix 'magit-status-jump '(0 0 -1)
      '("t " "Todos" magit-todos-jump-to-todos)))
  )


;; Highlight all the conflicted regions for git
(use-package smerge-mode
  :hook ((find-file . smerge-try-smerge))
  :config
  (defun smerge-try-smerge ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (require 'smerge-mode)
          (smerge-mode 1)))))
  )
