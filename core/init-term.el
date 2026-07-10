;;; -*- lexical-binding: t; -*-

(use-package ghostel
  :ensure t
  :bind
  (("C-t" . thy/ghostel-toggle-popup)
   ("C-c t" . thy/ghostel-transient)
   :map ghostel-semi-char-mode-map
   ("C-t" . thy/ghostel-toggle-popup)
   ("C-g" . keyboard-quit))
  :custom
  (ghostel-shell '("zsh"))
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
    (if-let ((window (thy/ghostel-visible-popup-window)))
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
  :config
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t))

(transient-define-prefix thy/ghostel-transient ()
  "Transient for Ghostel terminals."
  [("n" "New" thy/ghostel-new)
   ("l" "List" thy/ghostel-list-buffers)])
