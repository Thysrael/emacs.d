;;; -*- lexical-binding: t -*-

;; Modules can use this to skip local-only or remote-only packages.
(defvar thy/on-server
  (not (member (system-name)
               '("banana" "thysrael-82av" "jujube.local")))
  "Non-nil when this Emacs session is running on a remote/server host.")

(defvar thy/local-init-files
  '(
    init-package
    init-util
    init-tmp
    init-basic
    init-enhance
    init-window
    init-workspace
    init-navigate
    init-ui
    init-modeline
    init-input
    init-edit
    init-completion
    init-prog
    init-lang
    init-project
    init-vsc
    init-debug
    init-write
    init-term
    init-remote
    init-read
    init-dired
    ;; 'init-control
    init-note
    ;; init-ai
    init-modal
    ;; 'init-export
    )
  "Core modules loaded on local machines.")

(defvar thy/remote-init-files
  '(init-package
    init-util
    init-tmp
    init-basic
    init-enhance
    init-window
    init-workspace
    init-navigate
    init-ui
    init-modeline
    init-edit
    init-completion
    init-prog
    init-lang
    init-project
    init-vsc
    init-debug
    init-write
    init-remote
    init-dired
    ;; init-ai
    init-modal)
  "Core modules loaded on remote/server hosts.")

(defvar thy/init-files
  (if thy/on-server thy/remote-init-files thy/local-init-files)
  "Core modules selected for the current host.")

(let ((init-directory (expand-file-name "core/" user-emacs-directory)))
  (dolist (file thy/init-files)
    (load-file (concat init-directory (symbol-name file) ".el"))))
