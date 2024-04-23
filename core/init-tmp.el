;; (use-package holo-layer
;;   :straight (holo-layer :type git :host github :repo "manateelazycat/holo-layer"))

;; 果冻光标
;;(add-to-list 'load-path "~/.emacs.d/straight/repos/holo-layer")
;;(require 'holo-layer)
;;(setq holo-layer-enable-cursor-animation t)
;;(holo-layer-enable)

;; (use-package key-echo
;;   :straight (key-echo :type git :host github :repo "manateelazycat/key-echo"))

;; (add-to-list 'load-path "~/.emacs.d/straight/repos/key-echo/")
;; (require 'key-echo)
;; (setq key-echo-enable-debug t)
;;
;;
;; (defun key-echo-shift-to-switch-input-method (key)
;;   (interactive)
;;   (when (string-equal key "Key.shift")
;;     (toggle-input-method)
;;     ))
;;
;; (setq key-echo-single-key-trigger-func 'key-echo-shift-to-switch-input-method)
;;
;; (key-echo-enable)

;; (defun popper-display-in-posframe (buf _)
;;   (when (posframe-workable-p)
;;   (posframe-show buf
;;                  :position t
;;                  :poshandler #'posframe-poshandler-frame-center
;;                  :width 72
;;                  :height 25
;;                  :border-width 3
;;                  :border-color "IndianRed")))
;;
;; (setq popper-display-function #'popper-display-in-posframe)

;; (use-package tabspaces
;;   ;; use this next line only if you also use straight, otherwise ignore it.
;;   :straight (:type git :host github :repo "mclear-tools/tabspaces")
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
;;   :commands (tabspaces-switch-or-create-workspace
;;              tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   ;; sessions
;;   (tabspaces-session t))


;; [timeout] debounce and throttle
(use-package timeout
  :straight (:host github :repo "karthink/timeout" :branch "master"))

;; [zoom] Managing the window sizes automatically
;; (use-package zoom
;;   :hook (window-setup . zoom-mode)
;;   :config
;;   (timeout-throttle! 'zoom--handler 0.1))

;; (defun +patch/eglot-pyright-venv-workspace-config (server)
;;   `(:python\.analysis
;;     (:extraPaths ,(vector "~/learn/sem7/Isolation/gem5/src/python/"))))
;;
;; (setq-default eglot-workspace-configuration #'+patch/eglot-pyright-venv-workspace-config)

;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -interaction nonstopmode -output-directory %o %f"
;;         "xelatex -interaction nonstopmode -output-directory %o %f"))
;;
;; (with-eval-after-load 'ox-latex
;;   ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
;;   ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
;;   ;; automatically to resolve the cross-references.
;;   (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
;;   (add-to-list 'org-latex-classes
;;                '("elegantpaper"
;;                  "\\documentclass[lang=cn]{elegantpaper}
;;                    [NO-DEFAULT-PACKAGES]
;;                    [PACKAGES]
;;                    [EXTRA]"
;;                  ("\\section{%s}" . "\\section*{%s}")
;;                  ("\\subsection{%s}" . "\\subsection*{%s}")
;;                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
;;
;;   ;; 如果 org 设置了#+LATEX_HEADER: \usepackage{minted} 的话 则支持代码高亮和代码格式化
;;   (setq org-latex-listings 'minted)
;;   ;; Windows 加了这句的话 org 里 C-c C-x C-l 无法预览公式
;;   ;; 为了兼容性就不加这句 然后 org 里加上 #+LATEX_HEADER: \usepackage{minted} 即可
;;   ;; (add-to-list 'org-latex-packages-alist '("minted"))
;;
;;   ;; 解决 org 文件里面执行  org-latex-preview 生成的 .tex 格式不对的问题
;;   ;; 通过 log 可以看到错误信息 ! Package minted Error: You must invoke LaTeX with the -shell-escape flag
;;   )

;; (use-package prism
;;   :straight (prism :type git :host github :repo "alphapapa/prism.el"))

;; (use-package color-identifiers-mode
;;   :straight t)

;; 没有用，在边缘情况表现不是很好
;; (use-package iscroll
;;   :straight t)

;;  (defun my/org-insert-clipboard-image (width)
;;    "create a time stamped unique-named file from the clipboard in the sub-directory
;; (%filename.assets) as the org-buffer and insert a link to this file."
;;    (interactive (list
;;                  (read-string (format "Input image width, default is 800: ")
;;                               nil nil "800")))
;;    ;; 设置图片存放的文件夹位置为 `当前Org文件同名.assets'
;;    (setq foldername (concat (file-name-base (buffer-file-name)) ".assets/"))
;;    (if (not (file-exists-p foldername))
;;        (mkdir foldername))
;;    ;; 设置图片的文件名，格式为 `img_年月日_时分秒.png'
;;    (setq imgName (concat "img_" (format-time-string "%Y%m%d_%H%M%S") ".png"))
;;    ;; 图片文件的相对路径
;;    (setq relativeFilename (concat (file-name-base (buffer-name)) ".assets/" imgName))
;;    ;; 根据不同的操作系统设置不同的命令行工具
;;    (cond ((string-equal system-type "gnu/linux")
;;           (shell-command (concat "xclip -selection clipboard -t image/png -o > " relativeFilename)))
;;          ((string-equal system-type "darwin")
;;           (shell-command (concat "pngpaste " relativeFilename))))
;;    ;; 给粘贴好的图片链接加上宽度属性，方便导出
;;    (insert (concat "\n#+DOWNLOADED: screenshot @ "
;;                    (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time))
;;                    "\n#+CAPTION: \n#+ATTR_ORG: :width "
;;                    width
;;                    "\n#+ATTR_LATEX: :width "
;;                    (if (>= (/ (string-to-number width) 800.0) 1.0)
;;                        "1.0"
;;                      (number-to-string (/ (string-to-number width) 800.0)))
;;                    "\\linewidth :float nil\n"
;;                    "#+ATTR_HTML: :width "
;;                    width
;;                    "\n[[file:" relativeFilename "]]\n"))
;;    ;; 重新显示一下图片
;;    (org-redisplay-inline-images)
;;    )


;; (setq tree-sitter-load-path "~/.emacs.d/tree-sitter/")
;;
;; (use-package msgu
;;   :straight (msgu :type git :host github :repo "jcs-elpa/msgu"))
;;
;;
;; (use-package ts-docstr
;;   :straight (ts-docstr :type git :host github :repo "emacs-vs/ts-docstr"
;;                        :files (:defaults "langs/*.el"))
;;   :init
;;   (setq ts-docstr-key-support t))


;; (use-package org-tree-slide
;;   :straight t
;;   :bind
;;   (:map org-tree-slide-mode-map
;;         ("<f9>" . org-tree-slide-move-previous-tree)
;;         ("<f10>" . org-tree-slide-move-next-tree)
;;         )
;;   :hook
;;   (org-tree-slide-mode . (lambda () (setq-local org-num-mode -1)))
;;   :config
;;   (setq org-tree-slide-heading-emphasis t
;;         org-tree-slide-content-margin-top 1
;;         ))


;; 将 eldoc 展示在 box 而非 buffer 中
;; (use-package eldoc-box
;;   :hook
;;   (eglot-managed-mode . eldoc-box-hover-mode))


;; (use-package nano-vertico
;;   :straight (nano-vertico :type git :host github :repo "rougier/nano-vertico"))
