;;; -*- lexical-binding: t -*-
;;; Early startup optimizations.

;; Defer garbage collection during startup, then restore it after startup.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))
            (setq gc-cons-percentage 0.1)))

;; Native compilation settings.
(setq native-comp-jit-compilation t)
(setq native-comp-async-report-warnings-errors nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" user-emacs-directory)))

;; Skip site-wide startup files that are not used by this configuration.
(setq site-run-file nil)
(setq inhibit-default-init t)

;; Avoid legacy advice redefinition warnings during startup.
(setq ad-redefinition-action 'accept)

;; In noninteractive sessions, prefer newer source files.
(setq load-prefer-newer noninteractive)

;; Avoid expensive frame resizing during startup.
(setq frame-inhibit-implied-resize t)

;; macOS GUI: remove the title bar and window controls.
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'initial-frame-alist '(undecorated . t)))

;; Disable the default startup screen and keep the initial buffer minimal.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; Suppress redisplay during frame setup to reduce flicker.
(setq inhibit-redisplay t)
(setq inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq inhibit-redisplay nil)
            (setq inhibit-message nil)
            (redraw-frame)))

;; Let init.el initialize package.el explicitly.
(setq package-enable-at-startup nil)

;; Disable menu bar, tool bar, and scroll bars before the first frame appears.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

;; Avoid the second case-insensitive pass over `auto-mode-alist'.
(setq auto-mode-case-fold nil)

;; Temporarily disable file name handlers during startup.
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-value)))))))

;; Pretend Org optional modules have already been loaded.
;; https://emacs-china.org/t/org-babel/18699/10
(setq org-modules-loaded t)

;; (setenv "LSP_USE_PLISTS" "true")
