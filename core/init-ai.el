;;; -*- lexical-binding: t -*-

(defun thy/agent-shell-cwd ()
  "Return a project root for Agent Shell, or a private fallback workspace."
  (or (when-let* ((project (project-current)))
        (project-root project))
      (let ((workspace (no-littering-expand-var-file-name "agent-shell/workspace/")))
        (make-directory workspace t)
        workspace)))

(defun thy/agent-shell-cache-dir (&rest components)
  "Return an Agent Shell cache directory built from COMPONENTS under `no-littering'."
  (let ((dir (apply #'file-name-concat
                    (no-littering-expand-var-file-name "agent-shell/cache/")
                    components)))
    (make-directory dir t)
    dir))

(defun thy/agent-shell-set-buffer-face ()
  "Scale the Agent Shell buffer face to 90 percent."
  (face-remap-add-relative 'default :height 0.9))

(use-package acp
  :vc (acp :url "https://github.com/xenodium/acp.el" :rev :newest))

(use-package shell-maker
  :vc (shell-maker :url "https://github.com/xenodium/shell-maker" :rev :newest)
  :custom
  (shell-maker-root-path (no-littering-expand-var-file-name "shell-maker/"))
  (shell-maker-transcript-default-path (no-littering-expand-var-file-name "shell-maker/transcripts/")))

(use-package agent-shell
  :vc (agent-shell :url "https://github.com/xenodium/agent-shell" :rev :newest)
  :commands (agent-shell agent-shell-toggle)
  :bind ("C-o" . thy/agent-shell-toggle)
  :hook (agent-shell-mode . thy/agent-shell-set-buffer-face)
  :preface
  (defun thy/agent-shell-toggle (&optional arg)
    "Create an Agent Shell, or toggle an existing one.
With prefix ARG, delegate to `agent-shell'."
    (interactive "P")
    (if arg
        (agent-shell arg)
      (require 'agent-shell)
      (if (agent-shell-buffers)
          (agent-shell-toggle)
        (agent-shell))))
  :init
  (with-eval-after-load 'evil
    (evil-define-key* '(normal insert) 'global
      (kbd "C-o") #'thy/agent-shell-toggle))
  :config
  ;; Keep Agent Shell's fallback workspace and cache out of HOME.
  (setq agent-shell-cwd-function #'thy/agent-shell-cwd)
  (setq agent-shell-header-style 'text)
  (setq agent-shell-session-restore-verbosity 'full)
  (advice-add #'agent-shell-cache-dir :override #'thy/agent-shell-cache-dir)
  (require 'agent-shell-opencode)
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication :none t))
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config))
  (define-key agent-shell-mode-map (kbd "C-t") #'thy/ghostel-toggle-popup)
  (define-key agent-shell-mode-map (kbd "C-o") #'thy/agent-shell-toggle)
  (with-eval-after-load 'evil
    (evil-define-key '(normal insert) agent-shell-mode-map
      (kbd "C-t") #'thy/ghostel-toggle-popup
      (kbd "C-o") #'thy/agent-shell-toggle)))
