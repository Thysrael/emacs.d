;;; -*- lexical-binding: t; -*-

(use-package ghostel
  :ensure t
  :bind
  (("C-t" . thy/ghostel-toggle-popup)
   ("C-c t" . thy/ghostel-transient)
   :map ghostel-semi-char-mode-map
   ("C-g" . keyboard-quit))
  :custom
  (ghostel-module-directory
   (no-littering-expand-var-file-name "ghostel/"))
  (ghostel-shell '("zsh"))
  ;; Remote Ghostel terminals should use zsh too; this does not affect RPC jobs.
  (ghostel-tramp-shells '(("rpc" "zsh")
                          ("ssh" "zsh")
                          ("scp" "zsh")
                          ("docker" "/bin/sh")))
  :custom-face
  (ghostel-default ((t (:height 0.9))))
  :preface
  (defun thy/ghostel-visible-popup-window ()
    "Return the visible Ghostel popup window, if any."
    (catch 'window
      (dolist (entry (and (boundp 'popper-open-popup-alist)
                          popper-open-popup-alist))
        (let ((window (car entry))
              (buffer (cdr entry)))
          (when (and (window-live-p window)
                     (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (derived-mode-p 'ghostel-mode)))
            (throw 'window window))))))

  (defun thy/ghostel-toggle-popup ()
    "Show the project Ghostel popup, or hide it when already visible."
    (interactive)
    (if-let* ((window (thy/ghostel-visible-popup-window)))
        (delete-window window)
      (ghostel-project)))

  (defun thy/ghostel-new ()
    "Create a new Ghostel terminal for the current project."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'ghostel-project)))

  (defun thy/ghostel-list-buffers ()
    "Select a Ghostel buffer with live preview when Consult is available."
    (interactive)
    (let (names)
      (dolist (buffer (buffer-list))
        (when (with-current-buffer buffer
                (derived-mode-p 'ghostel-mode))
          (push (buffer-name buffer) names)))
      (setq names (nreverse names))
      (unless names
        (user-error "No Ghostel buffers"))
      (if (and (fboundp 'consult--read)
               (fboundp 'consult--buffer-preview))
          (let ((name (consult--read names
                                     :prompt "Ghostel: "
                                     :category 'buffer
                                     :state (consult--buffer-preview))))
            (pop-to-buffer name))
        (pop-to-buffer (completing-read "Ghostel: " names nil t)))))

  (defun thy/ghostel-bind-input-keys ()
    "Bind editing and control keys in every Ghostel input map."
    ;; Ghostel rebuilds some maps, so these keys must be re-applied.
    (dolist (map (list ghostel-char-mode-map
                       ghostel-line-mode-map
                       ghostel-semi-char-mode-map))
      (define-key map (kbd "M-c") #'ghostel-readonly-copy)
      (define-key map (kbd "M-v") #'ghostel-yank)
      (define-key map (kbd "C-t") #'thy/ghostel-toggle-popup)
      (define-key map (kbd "C-o") #'thy/agent-shell-toggle)
      (define-key map (kbd "C-c") #'ghostel-send-C-c))
    (with-eval-after-load 'evil
      (evil-define-key '(normal insert) ghostel-mode-map
        (kbd "M-c") #'ghostel-readonly-copy
        (kbd "M-v") #'ghostel-yank
        (kbd "C-t") #'thy/ghostel-toggle-popup
        (kbd "C-o") #'thy/agent-shell-toggle
        (kbd "C-c") #'ghostel-send-C-c)))
  :config
  (thy/ghostel-bind-input-keys)
  (advice-add #'ghostel--rebuild-semi-char-keymap
              :after (lambda (&rest _) (thy/ghostel-bind-input-keys)))
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t))

(use-package evil-ghostel
  :vc (evil-ghostel :url "https://github.com/dakra/ghostel"
                    :lisp-dir "extensions/evil-ghostel"
                    :rev :newest)
  :init
  (when-let* ((evil-ghostel-dir (expand-file-name "evil-ghostel/extensions/evil-ghostel"
                                                   package-user-dir))
              ((file-directory-p evil-ghostel-dir)))
    (add-to-list 'load-path evil-ghostel-dir))
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  (with-eval-after-load 'ghostel
    (thy/ghostel-bind-input-keys))
  (evil-define-key '(normal insert) evil-ghostel-mode-map
    (kbd "M-c") #'ghostel-readonly-copy
    (kbd "M-v") #'ghostel-yank
    (kbd "C-t") #'thy/ghostel-toggle-popup
    (kbd "C-o") #'thy/agent-shell-toggle
    (kbd "C-c") #'ghostel-send-C-c)
  (evil-define-key* 'insert evil-ghostel-mode-map
    (kbd "C-SPC") #'evil-force-normal-state))

(use-package ghostel-ime
  :ensure nil
  :after ghostel
  :preface
  (defun thy/ghostel-rime-composing-p (buffer)
    "Return non-nil when Rime is composing in Ghostel BUFFER."
    (with-current-buffer buffer
      (and (boundp 'rime--preedit-overlay)
           (overlayp rime--preedit-overlay)
           (eq (overlay-buffer rime--preedit-overlay) buffer))))

  (defun thy/ghostel-forward-rime-return (function &rest args)
    "Forward text inserted by Rime's return command to Ghostel's PTY."
    (if (not (and (derived-mode-p 'ghostel-mode)
                  (ghostel--terminal-input-mode-p)))
        (apply function args)
      (let ((before-point (point)))
        (let ((inhibit-read-only t))
          (apply function args))
        (when (> (point) before-point)
          (let ((inserted (buffer-substring-no-properties
                           before-point (point))))
            (let ((inhibit-read-only t))
              (delete-region before-point (point)))
            (ghostel--send-string
             (encode-coding-string inserted 'utf-8)))))))

  (defun thy/ghostel-enable-ime-support ()
    "Enable stable Rime input support in Ghostel."
    (when (fboundp 'sis-inline-mode)
      (sis-inline-mode -1))
    (when (fboundp 'sis-context-mode)
      (sis-context-mode -1))
    (ghostel-ime-mode 1)
    (add-hook 'ghostel-inhibit-redraw-functions
              #'thy/ghostel-rime-composing-p nil t))
  :hook (ghostel-mode . thy/ghostel-enable-ime-support)
  :config
  (advice-add #'rime--return :around #'thy/ghostel-forward-rime-return))

(transient-define-prefix thy/ghostel-transient ()
  "Transient for Ghostel terminals."
  [("n" "New" thy/ghostel-new)
   ("l" "List" thy/ghostel-list-buffers)])
