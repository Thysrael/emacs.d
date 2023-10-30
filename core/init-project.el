;;; -*- lexical-binding: t -*-

;;; 项目管理
(use-package project
  :bind
  ("C-x p" . hydra-project/body)
  :config
  (defhydra hydra-project (:color blue
                                  :hint nil
                                  :columns 5)
    "Project Commands"
    ("b" project-list-buffers "List Buffers" :color blue)
    ("x" project-execute-extended-command "Execute Extended Command" :color blue)
    ("r" project-query-replace-regexp "Query Replace Regexp" :color blue)
    ("G" project-or-external-find-regexp "Find Regexp (External)" :color blue)
    ("g" project-find-regexp "Find Regexp (Project)" :color blue)
    ("p" project-switch-project "Switch Project" :color blue)
    ("k" project-kill-buffers "Kill Buffers" :color blue)
    ("e" project-eshell "Eshell" :color blue)
    ("c" project-compile "Compile" :color blue)
    ("v" project-vc-dir "Version Control (VC) Dir" :color blue)
    ("D" project-dired "Dired" :color blue)
    ("d" project-find-dir "Find Directory" :color blue)
    ("s" project-shell "Shell" :color blue)
    ("S" project-switch-to-buffer "Switch to Buffer" :color blue)
    ("F" project-or-external-find-file "Find File (External)" :color blue)
    ("f" project-find-file "Find File" :color blue)
    ("&" project-async-shell-command "Async Shell Command" :color blue)
    ("!" project-shell-command "Shell Command" :color blue))
  )

(use-package treemacs
  :functions
  (treemacs-filewatch-mode treemacs-git-mode treemacs-delete-other-windows)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  (variable-pitch ((t nil))) ; 取消 treemacs 的默认字体
  :bind
  (("M-0"    . +switch-with-treemacs)
   ("<f6>"   . +treemacs-toggle)
   :map treemacs-mode-map
   ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  ;; Bind after load treemacs
  ;; (bind-keys :package treemacs
  ;;            ("C-x 1"     . treemacs-delete-other-windows))

  ;; treemacs-git-mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-case-insensitive-asc
        treemacs-follow-after-init       t
        treemacs-is-never-other-window   t
        treemacs-width                   30
        treemacs-no-png-images           nil)

  (treemacs-filewatch-mode t)
  (treemacs-follow-mode -1)

  (defun +treemacs-toggle ()
    (interactive)
    (require 'treemacs)
    (pcase (treemacs-current-visibility)
      (`visible (delete-window (treemacs-get-local-window)))
      (_ (if (project-current)
             (treemacs-add-and-display-current-project-exclusively)
           (treemacs)))))
  (defun +switch-with-treemacs()
    (interactive)
    (require 'treemacs)
    (if (not (eq (treemacs-current-visibility) `visible))
        (+treemacs-toggle)
      (if (eq (treemacs-get-local-window) (get-buffer-window))
          (other-window -1)
        (select-window (treemacs-get-local-window))
        )
      )
    )

  (custom-set-faces
   '(variable-pitch ((t nil))) ; 严格意义上说，取消 treemacs 的默认字体
   '(variable-pitch ((t (:height 0.9)))) ; 缩小默认字体
   )
  )
