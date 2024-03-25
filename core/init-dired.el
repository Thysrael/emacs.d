;;; -*- lexical-binding: t -*-

(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq
   ;; Always delete and copy recursively
   dired-recursive-deletes 'top
   dired-recursive-copies 'always
   ;; Move between two dired buffer quickly
   dired-dwim-target t
   ;; Ask whether destination dirs should get created when copying/removing files.
   dired-create-destination-dirs 'ask
   ;; symlink
   dired-hide-details-hide-symlink-targets nil
   dired-listing-switches "-alh"
   )
  )

(use-package dired-aux
  :straight nil
  :after dired
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))


;; [dired-x] Extra Dired functionality
(use-package dired-x
  :straight nil
  :after dired
  :bind (:map dired-mode-map
              ("." . dired-omit-mode))
  :config
  (let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
                   ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
                   ((and (eq system-type 'windows-nt) (display-graphic-p)) "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))

  (setq dired-omit-verbose nil
        ;; hide dot files
        ;;dired-omit-files "^\\..*\\'"
        )

  ;; Disable the prompt about killing the Dired buffer for a deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  )

;; 按 v 显示 git commit 信息
(use-package dired-git-info
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("v" . dired-git-info-mode))
  :config
  (setq dgi-commit-message-format "%h %cs %s"
        dgi-auto-hide-details-p nil)
  )

;; 图标
(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; 按 tab 显示子树
(use-package dired-hacks
  :straight (:files (:defaults "*.el"))
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle))
  :init
  ;; Don't show background, which is ugly in light themes
  (with-eval-after-load 'dired-subtree
    (setq dired-subtree-use-backgrounds nil)))

;; dired more colorful
(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode))
