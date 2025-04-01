(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window auctex auto-dim-other-buffers beginend blamer breadcrumb cal-china-x cape cdlatex
                chatgpt-shell cns code-cells consult-dir consult-eglot consult-org-roam corfu
                cuda-mode diff-hl diredfl dirvish doom-themes dts-mode dumb-jump eaf eglot-booster
                embark-consult embrace engrave-faces esup forge gcmh gnuplot gnuplot-mode hl-todo
                indent-bars kconfig-mode lua-mode marginalia meow mwim nerd-icons-completion
                nerd-icons-corfu no-littering orderless org-appear org-autolist pangu-spacing
                pinyinlib popper puni quickrun rainbow-delimiters rainbow-mode rime rust-mode
                scala-mode sdcv shackle sis sudo-edit symbol-overlay tempel treesit-auto
                typescript-mode vertico visual-fill-column vterm vundo wgrep ws-butler yaml-mode))
 '(package-vc-selected-packages
   '((eaf :url "https://github.com/emacs-eaf/emacs-application-framework" :shell-command
          "python install-eaf.py --install pdf-viewer --ignore-sys-deps")
     (sdcv :url "https://github.com/manateelazycat/sdcv")
     (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
     (cns :url "https://github.com/kanglmf/emacs-chinese-word-segmentation.git" :shell-command
          "git submodule update --init --recursive" :make "all"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:inherit (font-lock-builtin-face) :weight ultra-bold))))
 '(fixed-pitch ((t (:family "JetBrainsMono Nerd Font"))))
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face :slant italic))))
 '(font-lock-number-face ((t (:foreground unspecified :inherit (font-lock-constant-face)))))
 '(font-lock-preprocessor-face ((t (:foreground unspecified :family "JetBrainsMono Nerd Font"))))
 '(font-lock-property-name-face ((t (:inherit font-lock-variable-name-face :slant italic))))
 '(font-lock-property-use-face ((t (:inherit font-lock-value-use-face :slant italic))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground unspecified))))
 '(font-lock-variable-use-face ((t (:inherit unspecified))))
 '(variable-pitch ((t (:height 0.9 :family "Symbols Nerd Font Mono")))))
