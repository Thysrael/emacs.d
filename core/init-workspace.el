;;; -*- lexical-binding: t; -*-

;; Manage workspaces with the built-in tab bar.
(use-package tab-bar
  :ensure nil
  :bind
  (("M-t" . tab-new)
   ("M-w" . tab-close))
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
    "Return the current tab name with its remote host and window count."
    (let* ((buffer (window-buffer (minibuffer-selected-window)))
           (raw-tab-name (buffer-name buffer))
           (hostname
            (with-current-buffer buffer
              (and default-directory
                   (file-remote-p default-directory 'host))))
           (remote-name
            (and hostname
                 (not (string-equal hostname "localhost"))
                 (concat " @" hostname)))
           (count (length (window-list-1 nil 'nomini))))
      (concat raw-tab-name
              remote-name
              (and (> count 1) (format "(%d)" count)))))

  (defun thy/tab-bar-tab-name-format (tab index)
    "Return TAB with one leading space before its default label."
    (let ((face (funcall tab-bar-tab-face-function tab)))
      (concat (propertize " " 'face face)
              (tab-bar-tab-name-format-default tab index))))
  :config
  ;; Keep tab switching ahead of mode-specific Meta bindings.
  (keymap-global-set "<thy-tab-next>" #'tab-next)
  (keymap-global-set "<thy-tab-previous>" #'tab-previous)
  (define-key key-translation-map (kbd "M-j") (kbd "<thy-tab-next>"))
  (define-key key-translation-map (kbd "M-k") (kbd "<thy-tab-previous>"))

  (custom-theme-set-faces
   'user
   '(tab-bar ((t (:inherit mode-line-inactive :box nil))))
   '(tab-bar-tab
     ((t (:inherit mode-line
          :foreground "#f8f8f2" :background "#6272a4"
          :weight bold :underline nil :box nil))))
   '(tab-bar-tab-inactive
     ((t (:inherit mode-line-inactive
          :weight normal :underline nil :box nil))))))
