


;; (define-key global-map (kbd "C-c") nil)
;; (define-key global-map (kbd "C-o") (lookup-key global-map (kbd "C-c")))
;; (cua-mode t)

;; 这是改变输入法，还没有尝试
;; (defvar input-toggle nil "Toggle variable for input method")

;; (defun fcitx2en ()
;;   (let ((input-status (shell-command-to-string "fcitx-remote")))
;;     (when (= (string-to-number input-status) 1)
;;       (setq input-toggle nil)
;;       (shell-command "fcitx-remote -o"))))

;; (defun fcitx2zh ()
;;   (let ((input-status (shell-command-to-string "fcitx-remote")))
;;     (unless (and (not (= (string-to-number input-status) 1)) (not input-toggle))
;;       (shell-command "fcitx-remote -c")
;;       (setq input-toggle t))))

;; (defun fcitx-on-focus-gained ()
;;   (if (equal (selected-window) (minibuffer-window))
;;       (fcitx2en)
;;     (fcitx2zh)))

;; (defun fcitx-on-focus-lost ()
;;   (fcitx2zh))

;; (defun fcitx-on-emacs-exit ()
;;   (fcitx2zh))

;; (setq focus-in-hook 'fcitx-on-focus-gained)
;; (setq focus-out-hook 'fcitx-on-focus-lost)
;; (add-hook 'kill-emacs-hook 'fcitx-on-emacs-exit)