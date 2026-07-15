;;; -*- lexical-binding: t -*-

(defun thy/agent-shell-cwd ()
  "Return the current project root or `default-directory' for Agent Shell."
  (or (when-let* ((project (project-current)))
        (project-root project))
      default-directory))

(defun thy/agent-shell-cache-dir (&rest components)
  "Return an Agent Shell cache directory built from COMPONENTS under `no-littering'."
  (let ((dir (apply #'file-name-concat
                    (no-littering-expand-var-file-name "agent-shell/cache/")
                    components)))
    (make-directory dir t)
    dir))

(defun thy/agent-shell-data-dir (subdir)
  "Return Agent Shell SUBDIR under `no-littering'."
  (no-littering-expand-var-file-name
   (file-name-concat "agent-shell" subdir)))

(defun thy/agent-shell-set-buffer-face ()
  "Scale the Agent Shell buffer face to 90 percent."
  (face-remap-add-relative 'default :height 0.9))

(defun thy/agent-shell-enable-corfu ()
  "Use Corfu for Agent Shell's explicit completion requests."
  (setq-local corfu-auto nil)
  (corfu-mode 1))

(defun thy/agent-shell-default-high-effort ()
  "Set OpenCode sessions to high reasoning effort after initialization."
  (agent-shell-subscribe-to
   :shell-buffer (current-buffer)
   :event 'init-session
   :on-event
   (lambda (_event)
     (when-let* (((eq (map-nested-elt (agent-shell--state)
                                      '(:agent-config :identifier))
                      'opencode))
                 (levels (agent-shell--get-available-thought-levels
                          (agent-shell--state)))
                 ((seq-some (lambda (level)
                              (equal (map-elt level :value) "high"))
                            levels))
                 ((not (equal (agent-shell--current-thought-level-id
                               (agent-shell--state))
                              "high"))))
       (agent-shell--config-option-set-thought-level-id
        :thought-level-id "high")))))

(defun thy/agent-shell-read-buffer (prompt)
  "Read an Agent Shell buffer with Consult using PROMPT."
  (require 'agent-shell)
  (require 'consult)
  (let ((buffers (agent-shell-buffers)))
    (unless buffers
      (user-error "No Agent Shell buffers"))
    (get-buffer
     (consult--read (mapcar #'buffer-name buffers)
                    :prompt prompt
                    :category 'buffer
                    :state (consult--buffer-preview)
                    :sort nil
                    :require-match t))))

(defun thy/consult-agent-shell-buffer ()
  "Switch to an Agent Shell buffer with Consult preview."
  (interactive)
  (let ((shell-buffer
         (thy/agent-shell-read-buffer "Switch to Agent Shell: ")))
    (switch-to-buffer
     (or (when agent-shell-prefer-viewport-interaction
           (agent-shell-viewport--buffer
            :shell-buffer shell-buffer
            :existing-only t))
         shell-buffer))))

(defun thy/agent-shell-kill-buffer ()
  "Select and kill an Agent Shell buffer."
  (interactive)
  (kill-buffer (thy/agent-shell-read-buffer "Kill Agent Shell: ")))

(defun thy/agent-shell-restart ()
  "Restart the current Agent Shell, selecting one when necessary."
  (interactive)
  (if (derived-mode-p 'agent-shell-mode)
      (agent-shell-restart)
    (with-current-buffer (thy/agent-shell-read-buffer "Restart Agent Shell: ")
      (agent-shell-restart))))

(use-package acp
  :vc (acp :url "https://github.com/xenodium/acp.el" :rev :newest))

(use-package shell-maker
  :vc (shell-maker :url "https://github.com/xenodium/shell-maker" :rev :newest)
  :custom
  (shell-maker-root-path (no-littering-expand-var-file-name "shell-maker/"))
  (shell-maker-transcript-default-path (no-littering-expand-var-file-name "shell-maker/transcripts/")))

(use-package agent-shell
  :vc (agent-shell :url "https://github.com/xenodium/agent-shell" :rev :newest)
  :commands (agent-shell agent-shell-insert-file agent-shell-new-shell
                          agent-shell-toggle)
  :bind ("C-o" . thy/agent-shell-toggle)
  :hook
  ((agent-shell-mode . thy/agent-shell-set-buffer-face)
   (agent-shell-mode . thy/agent-shell-enable-corfu)
   (agent-shell-mode . thy/agent-shell-default-high-effort))
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
  ;; Keep Agent Shell's generated data under no-littering.
  (setq agent-shell-cwd-function #'thy/agent-shell-cwd)
  (setq agent-shell-dot-subdir-function #'thy/agent-shell-data-dir)
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

(transient-define-prefix thy/agent-shell-transient ()
  "Transient for Agent Shell sessions."
  [[("n" "New" agent-shell-new-shell)
    ("l" "List" thy/consult-agent-shell-buffer)
    ("k" "Kill" thy/agent-shell-kill-buffer)]
   [("r" "Restart" thy/agent-shell-restart)
    ("f" "File" agent-shell-insert-file)]])
