;;; -*- lexical-binding: t -*-

(use-package project
  :ensure nil
  :custom
  (project-vc-merge-submodules nil))

(use-package treemacs
  :ensure t
  :commands (treemacs treemacs-select-window)
  :preface
  (defun thy/treemacs-toggle ()
    "Toggle Treemacs for the current project or directory."
    (interactive)
    (require 'treemacs)
    (if (eq (treemacs-current-visibility) 'visible)
        (delete-window (treemacs-get-local-window))
      (unless (treemacs-current-workspace)
        (treemacs-do-create-workspace "Default"))
      (treemacs-add-and-display-current-project-exclusively)))
  :bind
  (("<f6>" . thy/treemacs-toggle)
   ("M-0" . treemacs-select-window)
   :map treemacs-mode-map
   ([mouse-1] . treemacs-single-click-expand-action))
  :custom
  (treemacs-follow-after-init t)
  (treemacs-indentation 1)
  (treemacs-is-never-other-window t)
  (treemacs-missing-project-action 'remove)
  (treemacs-no-png-images t)
  (treemacs-persist-file
   (no-littering-expand-var-file-name "treemacs-persist"))
  (treemacs-last-error-persist-file
   (no-littering-expand-var-file-name "treemacs-persist-at-last-error"))
  (treemacs-sorting 'alphabetic-case-insensitive-asc)
  (treemacs-space-between-root-nodes nil)
  (treemacs-width 36)
  :custom-face
  (treemacs-git-modified-face
   ((t (:inherit thy/vc-change-face :foreground unspecified))))
  (treemacs-git-renamed-face
   ((t (:inherit thy/vc-change-face :foreground unspecified))))
  (treemacs-git-added-face
   ((t (:inherit thy/vc-insert-face :foreground unspecified))))
  (treemacs-git-untracked-face
   ((t (:inherit thy/vc-insert-face :foreground unspecified))))
  (treemacs-git-conflict-face
   ((t (:inherit thy/vc-delete-face :foreground unspecified))))
  (treemacs-git-ignored-face
   ((t (:inherit dired-ignored :foreground unspecified))))
  :config
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0))
  (treemacs-filewatch-mode 1)
  (treemacs-follow-mode 1)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  ;; This demand is relative to Treemacs and does not load it at startup.
  :demand t
  :preface
  (defun thy/treemacs-compact-nerd-icons ()
    "Remove unused leading slots from Nerd Icons in the current theme."
    (dolist (icons (list (treemacs-theme->gui-icons treemacs--current-theme)
                         (treemacs-theme->tui-icons treemacs--current-theme)))
      (maphash
       (lambda (key icon)
         (cond
          ((and (or (memq key '(dir-open dir-closed))
                    (and (stringp key)
                         (string-match-p "-\\(?:open\\|closed\\)\\'" key)))
                (string-match "\t" icon))
           (puthash key (substring icon (match-end 0)) icons))
          ((string-prefix-p " \t" icon)
           (puthash key (substring icon 2) icons))))
       icons)))
  :custom
  (treemacs-nerd-icons-icon-size 1.0)
  :custom-face
  (treemacs-nerd-icons-root-face
   ((t (:inherit nerd-icons-blue :height 1.1))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-blue))))
  :config
  (treemacs-nerd-icons-config)
  (thy/treemacs-compact-nerd-icons))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil)
  ;; Load the integration as soon as both deferred dependencies are present.
  :demand t)
