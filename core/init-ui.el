;;; -*- lexical-binding: t -*-

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  (dolist (icon '(("drawio" nerd-icons-mdicon "nf-md-drawing" :face nerd-icons-lpink)
                  ("rst" nerd-icons-mdicon "nf-md-file_document" :face nerd-icons-lpink)
                  ("ets" nerd-icons-sucicon "nf-seti-typescript" :face nerd-icons-blue-alt)))
    (push icon nerd-icons-extension-icon-alist))
  (dolist (icon '((conf-space-mode nerd-icons-codicon "nf-cod-settings" :face nerd-icons-lyellow)
                  (pdf-outline-buffer-mode nerd-icons-mdicon "nf-md-view_list" :face nerd-icons-dred)
                  (pdf-occur-buffer-mode nerd-icons-mdicon "nf-md-view_list" :face nerd-icons-dred)
                  (gfm-view-mode nerd-icons-octicon "nf-oct-markdown" :face nerd-icons-lblue)))
    (push icon nerd-icons-mode-icon-alist))
  (push '("^config$" nerd-icons-codicon "nf-cod-settings" :face nerd-icons-lyellow)
        nerd-icons-regexp-icon-alist))

;; UI responsiveness.
(setq idle-update-delay 1.0
      redisplay-skip-fontification-on-input t
      highlight-nonselected-windows nil
      inhibit-compacting-font-caches t)

;; Scrolling.
(setq fast-but-imprecise-scrolling t
      scroll-step 0
      scroll-margin 5
      scroll-conservatively 101
      auto-window-vscroll nil
      auto-hscroll-mode t
      hscroll-step 0
      hscroll-margin 2)

(use-package pixel-scroll
  :ensure nil
  :demand t
  :config
  (pixel-scroll-precision-mode 1))

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((control) . nil))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))

;; Fringes and continuation indicators.
(when (display-graphic-p)
  (fringe-mode '(5 . 8))
  (define-fringe-bitmap 'right-curly-arrow
    [#b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000
     #b00000000
     #b00110000
     #b00110000])
  (define-fringe-bitmap 'right-arrow
    [#b00000000
     #b00000000
     #b00001110
     #b00001110
     #b00001110
     #b00000000
     #b00000000
     #b00000000])
  (define-fringe-bitmap 'left-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b01110000
     #b01110000
     #b01110000
     #b00000000
     #b00000000]))

(defun thy/font-size ()
  "Return the preferred font size for the current machine."
  (if (string-equal (system-name) "banana") 28 14))

(defvar thy/font-size-adjustment 0
  "Runtime font size adjustment applied on top of `thy/font-size'.")

(defun thy/current-font-size ()
  "Return the current font size including runtime adjustment."
  (max 8 (+ (thy/font-size) thy/font-size-adjustment)))

(defun thy/setup-fonts ()
  "Set up default, CJK, and symbol fonts for graphical frames."
  (when (display-graphic-p)
    (let ((font-size (thy/current-font-size)))
      (set-face-attribute 'default nil :font (font-spec :family "Maple Mono NF CN" :size font-size))
      (set-fontset-font t 'han (font-spec :family "Maple Mono NF CN"))
      (set-fontset-font t 'han (font-spec :script 'han) nil 'append)
      (set-fontset-font t 'cjk-misc (font-spec :family "Maple Mono NF CN"))
      (set-fontset-font t '(#xe000 . #xf8ff) (font-spec :family "Symbols Nerd Font Mono") nil 'prepend)
      (set-fontset-font t '(#xf0000 . #xffffd) (font-spec :family "Symbols Nerd Font Mono") nil 'prepend))))

(defun thy/adjust-font-size (increment)
  "Adjust English and CJK font sizes globally by INCREMENT."
  (interactive "p")
  (setq thy/font-size-adjustment (+ thy/font-size-adjustment increment))
  (thy/setup-fonts))

(defun thy/reset-font-size ()
  "Reset English and CJK font sizes to the machine default."
  (interactive)
  (setq thy/font-size-adjustment 0)
  (thy/setup-fonts))

(thy/setup-fonts)
(add-hook 'server-after-make-frame-hook #'thy/setup-fonts)

;; Scale fonts globally; Popper intentionally does not claim these keys.
(global-set-key (kbd "C-=") #'thy/adjust-font-size)
(global-set-key (kbd "C--") (lambda () (interactive) (thy/adjust-font-size -1)))
(global-set-key (kbd "C-0") #'thy/reset-font-size)

(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Keep only the right divider; the mode line separates windows vertically.
(setq window-divider-default-places t
      window-divider-default-bottom-width 0
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Let maximized frames fill the screen exactly.
(setq frame-resize-pixelwise t)

(add-to-list 'default-frame-alist '(alpha-background . 98))

;; Minibuffer.
(setq enable-recursive-minibuffers t
      echo-keystrokes 0.02
      minibuffer-prompt-properties '(read-only t
                                               intangible t
                                               cursor-intangible t
                                               face minibuffer-prompt)
      epg-pinentry-mode 'loopback)
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq frame-title-format
      '((:eval (or buffer-file-truename "%b"))
        (" · Emacs")))

(setq initial-frame-alist '((fullscreen . maximized)))

(setq custom-safe-themes t)

(use-package doom-themes
  :ensure t
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (load-theme 'doom-dracula t))

(custom-set-faces
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face :slant italic))))
 '(font-lock-number-face ((t (:foreground unspecified :inherit (font-lock-constant-face)))))
 '(font-lock-property-name-face ((t (:inherit font-lock-variable-name-face :slant italic))))
 '(font-lock-property-use-face ((t (:inherit font-lock-value-use-face :slant italic))))
 '(font-lock-variable-use-face ((t (:inherit unspecified))))
 '(font-lock-preprocessor-face ((t (:foreground unspecified :family "Maple Mono NF CN"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground unspecified))))
 '(fixed-pitch ((t (:family "Maple Mono NF CN"))))
 '(variable-pitch ((t (:height 0.9 :family "Symbols Nerd Font Mono"))))
 '(bold ((t (:inherit (font-lock-builtin-face) :weight ultra-bold)))))
