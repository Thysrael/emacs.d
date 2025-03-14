;;; -*- lexical-binding: t -*-

(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  ;; Move between two dired buffer quickly
  (dired-dwim-target t)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (dired-create-destination-dirs 'ask)
  ;; symlink
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-Bhl --group-directories-first --almost-all")
  (dired-vc-rename-file t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  :config
  (setq delete-by-moving-to-trash t)
  )

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :custom-face
  (dirvish-hl-line ((t (:inherit hl-line))))
  (dirvish-collapse-file-face ((t (:height 0.8))))
  (dirvish-collapse-dir-face ((t (:inherit dired-directory :height 0.8))))
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/" "Home")
     ("d" "~/Desktop/" "Desktop")
     ("p" "~/project/" "Project")
     ("b" "~/blog/" "Blog")
     ("r" "~/roam/" "Roam")
     ("l" "~/learn/" "Learn")
     ))
  ;; dirvish 底栏
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (file-time index)))
  (dirvish-side-mode-line-format
   '(:right (yank file-size index)))
  ;; dirvish 条目
  ;; collapse 是对于目录的折叠
  (dirvish-attributes
   '(vc-state nerd-icons collapse git-msg file-size))
  ;; dirvish 侧边栏条目
  (dirvish-side-attributes
   '(vc-state nerd-icons collapse))
  ;; dirvish 顶栏
  (dirvish-header-line-format
   '(:left (path) :right (omit yank vc-info free-space)))
  (dirvish-path-separators '("~" "/" "/"))
  (dirvish-window-fringe 4)
  (dirvish-hide-cursor t)
  ;; (dirvish-hide-details '(dirvish-side))
  :bind
  (("<f9>" . dirvish)
   ("<f6>" . dirvish-side)
   :map dirvish-mode-map          ; Dirvish inherits `dired-mode-map'
   ("?"   . dirvish-dispatch)     ; contains most of sub-menus in dirvish extensions
   ;; 导航
   ("a"   . dirvish-quick-access)
   ("r"   . dirvish-history-jump)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ;; fd
   ("f"   . dirvish-fd)
   ("F"   . dirvish-fd-switches)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("<"   . dired-up-directory)
   (">"   . dired-find-file)
   ;; 快速排序
   ("s"   . dirvish-quicksort)
   ;; 快速标记
   ("M" . dirvish-mark-menu)
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  :hook
  (dirvish-mode . dired-omit-mode)
  )

;; [dired-x] Extra Dired functionality
;; 主要使用忽略（omit）功能
(use-package dired-x
  :bind (:map dired-mode-map
              ("." . dired-omit-mode))
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; (use-package dired-x
;;   :straight nil
;;   :after dired
;;   :bind (:map dired-mode-map
;;               ("." . dired-omit-mode))
;;   :config
;;   (let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
;;                    ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
;;                    ((and (eq system-type 'windows-nt) (display-graphic-p)) "start")
;;                    (t ""))))
;;     (setq dired-guess-shell-alist-user
;;           `(("\\.pdf\\'" ,cmd)
;;             ("\\.docx\\'" ,cmd)
;;             ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
;;             ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
;;             ("\\.\\(?:xcf\\)\\'" ,cmd)
;;             ("\\.csv\\'" ,cmd)
;;             ("\\.tex\\'" ,cmd)
;;             ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
;;             ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))
;;
;;   (setq dired-omit-verbose nil
;;         ;; hide dot files
;;         ;;dired-omit-files "^\\..*\\'"
;;         )
;;
;;   ;; Disable the prompt about killing the Dired buffer for a deleted directory.
;;   (setq dired-clean-confirm-killing-deleted-buffers nil)
;;   )

;; dired more colorful
(use-package diredfl
  :straight t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))
