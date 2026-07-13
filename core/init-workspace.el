;;; -*- lexical-binding: t; -*-

;; Manage workspaces with the built-in tab bar.
(use-package tab-bar
  :ensure nil
  :bind
  (("M-t" . tab-new)
   ("M-w" . tab-close)
   ("M-k" . tab-next)
   ("M-j" . tab-previous)
   :map tab-bar-mode-map
   ("C-<tab>" . nil))
  :custom
  (tab-bar-auto-width t)
  (tab-bar-auto-width-max nil)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-button-show nil)
  (tab-bar-separator "")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'thy/tab-bar-tab-name-format)
  (tab-bar-tab-name-function #'thy/tab-bar-tab-name)
  :preface
  (defun thy/tab-bar-tab-name ()
    "Return the current tab name with its window count when needed."
    (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
           (count (length (window-list-1 nil 'nomini))))
      (if (> count 1)
          (format "%s(%d)" raw-tab-name count)
        raw-tab-name)))

  (defun thy/tab-bar-tab-name-format (tab index)
    "Return TAB with one leading space before its default label."
    (let ((face (funcall tab-bar-tab-face-function tab)))
      (concat (propertize " " 'face face)
              (tab-bar-tab-name-format-default tab index)))))
