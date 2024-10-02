;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :straight t
  :config
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+)
  :bind
  (:map c-mode-map
        ("C-d" . nil))
  (:map c++-mode-map
        ("C-d" . nil))
  )

(use-package c-mode
  :hook
  (c-mode . (lambda () (c-toggle-comment-style))) ;; 改变默认注释风格
  )

(use-package c++-mode
  :mode "\\.isa\\'"
  :bind
  (:map c++-mode-map
        ("C-d" . nil)))

;; (use-package c-ts-mode
;;   :straight t
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.isa\\'" . c++-ts-mode))
;;   :config
;;   (setq c-ts-mode-indent-offset 4)
;;   (setq c-ts-mode-indent-style 'gnu)
;;   )

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  )

(use-package js
  :straight t
  :config
  (setq js-indent-level 2))

(use-package verilog-mode
  :config
  (setq verilog-align-ifelse t
        verilog-auto-delete-trailing-whitespace t
        verilog-auto-inst-param-value t
        verilog-auto-inst-vector nil
        verilog-auto-lineup (quote all)
        verilog-auto-newline nil
        verilog-auto-save-policy nil
        verilog-auto-template-warn-unused t
        verilog-case-indent 4
        verilog-cexp-indent 4
        verilog-highlight-grouping-keywords t
        verilog-highlight-modules t
        verilog-indent-level 4
        verilog-indent-level-behavioral 4
        verilog-indent-level-declaration 4
        verilog-indent-level-module 4
        verilog-tab-to-comment t))

(use-package scala-mode
  :straight t
  :config
  (setq
   scala-indent:align-parameters t
   ;; indent block comments to first asterix, not second
   scala-indent:use-javadoc-style t))

(use-package python-mode
  :straight nil
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("SConsopts\\'" . python-mode))
  ;; :mode ("SConstruct\\'" . python-mode)
  ;; :mode ("SConscript\\'" . python-mode)
  ;; :mode ("SConsopts\\'" . python-mode)
  :init
  (setq python-indent-offset 4)
  )

(use-package cuda-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package typescript-mode
  :straight t
  :config
  (setq typescript-indent-level 2))

(use-package dts-mode
  :straight t)

(use-package kconfig-mode
  :straight t)

(use-package lua-mode
  :straight t)

(use-package gnuplot-mode
  :straight t
  :mode ("\\.gp$" . gnuplot-mode)
  )

(use-package rust-mode
  :straight t
  )

;; (use-package cargo
;;   :straight t
;;   :hook ((rust-mode) . cargo-minor-mode))

(use-package conf-mode
  :mode
  (
   ("\\.service$" . conf-mode)
   ("\\.path$" . conf-mode)
   ("\\.target$" . conf-mode)
   ("\\.socket$" . conf-mode)
   ))
