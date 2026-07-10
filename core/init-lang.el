;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :ensure nil
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :bind
  (:map c-mode-map
        ("C-d" . nil))
  (:map c++-mode-map
        ("C-d" . nil))
  :config
  (c-set-offset 'case-label '+))

(use-package c-mode
  :ensure nil
  :hook
  (c-mode . (lambda () (c-toggle-comment-style))) ;; 改变默认注释风格
  )

(use-package c++-mode
  :ensure nil
  :mode "\\.isa\\'"
  :bind
  (:map c++-mode-map
        ("C-d" . nil)))

(use-package c-ts-mode
  :ensure nil
  :custom
  (c-ts-common-indent-offset 4)
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'linux)
  :hook
  ((c-ts-mode c++-ts-mode) . c-ts-mode-toggle-comment-style)
  )

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  )

(use-package js
  :ensure nil
  :custom
  (js-indent-level 2))

(use-package verilog-mode
  :custom
  (verilog-align-ifelse t)
  (verilog-auto-delete-trailing-whitespace t)
  (verilog-auto-inst-param-value t)
  (verilog-auto-inst-vector nil)
  (verilog-auto-lineup 'all)
  (verilog-auto-newline nil)
  (verilog-auto-save-policy nil)
  (verilog-auto-template-warn-unused t)
  (verilog-case-indent 4)
  (verilog-cexp-indent 4)
  (verilog-highlight-grouping-keywords t)
  (verilog-highlight-modules t)
  (verilog-indent-level 4)
  (verilog-indent-level-behavioral 4)
  (verilog-indent-level-declaration 4)
  (verilog-indent-level-module 4)
  (verilog-tab-to-comment t))

(use-package scala-mode
  :ensure t
  :custom
  (scala-indent:align-parameters t)
  ;; Indent block comments to the first asterisk, not the second.
  (scala-indent:use-javadoc-style t))

(use-package python-mode
  :ensure nil
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("SConsopts\\'" . python-mode))
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset-verbose nil)
  )

(use-package cuda-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package typescript-mode
  :mode (("\\.ets\\'" . typescript-ts-mode))
  :ensure t
  :custom
  (typescript-indent-level 2))

(use-package dts-mode
  :ensure t)

(use-package kconfig-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package gnuplot-mode
  :ensure t
  :mode ("\\.gp$" . gnuplot-mode)
  )

(use-package rust-mode
  :ensure t)

(use-package conf-mode
  :ensure nil
  :mode
  (("\\.service$" . conf-mode)
   ("\\.path$" . conf-mode)
   ("\\.target$" . conf-mode)
   ("\\.socket$" . conf-mode)
   ("\\.inf$" . conf-mode)
   ("\\.dsc$" . conf-mode)))
