;;; -*- lexical-binding: t -*-

(autoload 'agent-shell-openai-start-codex "agent-shell-openai" nil t)

(defun thy/agent-shell-cwd ()
  "Return a project root for Agent Shell, or a private fallback workspace."
  (or (when-let* ((project (project-current)))
        (project-root project))
      (let ((workspace (no-littering-expand-var-file-name "agent-shell/workspace/")))
        (make-directory workspace t)
        workspace)))

(defun thy/agent-shell-cache-dir (&rest components)
  "Return an Agent Shell cache directory under `no-littering'."
  (let ((dir (apply #'file-name-concat
                    (no-littering-expand-var-file-name "agent-shell/cache/")
                    components)))
    (make-directory dir t)
    dir))

(defun thy/gptel-horizon-api-key ()
  "Return the Horizon OpenAI-compatible API key from the environment."
  (or (getenv "OPENAI_HORIZON_API_KEY")
      (user-error "OPENAI_HORIZON_API_KEY is not set")))

(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu)
  :config
  ;; Keep credentials out of this config; export OPENAI_HORIZON_API_KEY instead.
  (require 'gptel-openai-responses)
  (setq gptel-model 'gpt-5.4-mini)
  (setq gptel-backend
        (gptel-make-openai-responses "OpenAI"
          :host "api.horizon1123.top"
          :key #'thy/gptel-horizon-api-key
          :stream t)))

(use-package acp
  :vc (acp :url "https://github.com/xenodium/acp.el" :rev :newest))

(use-package shell-maker
  :vc (shell-maker :url "https://github.com/xenodium/shell-maker" :rev :newest)
  :custom
  (shell-maker-root-path (no-littering-expand-var-file-name "shell-maker/"))
  (shell-maker-transcript-default-path (no-littering-expand-var-file-name "shell-maker/transcripts/")))

(use-package agent-shell
  :vc (agent-shell :url "https://github.com/xenodium/agent-shell" :rev :newest)
  :after (acp shell-maker)
  :commands (agent-shell)
  :config
  ;; Keep Agent Shell's fallback workspace and cache out of HOME.
  (setq agent-shell-cwd-function #'thy/agent-shell-cwd)
  (advice-add #'agent-shell-cache-dir :override #'thy/agent-shell-cache-dir)
  ;; Codex defaults to login-based OpenAI authentication.
  (require 'agent-shell-openai)
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))
  (setq agent-shell-preferred-agent-config
        (agent-shell-openai-make-codex-config)))
