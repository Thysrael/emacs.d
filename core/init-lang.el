;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :config
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+))

;; (use-package cmake-mode)

(use-package js
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
  :config
  (setq
   scala-indent:align-parameters t
   ;; indent block comments to first asterix, not second
   scala-indent:use-javadoc-style t))

(use-package python-mode
  :straight nil
  :config
  (setq python-indent-offset 4)
  (add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
  (add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))
)

(use-package cuda-mode)
