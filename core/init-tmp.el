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

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(with-eval-after-load 'ox-latex
  ;; http://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
  ;; latexmk runs pdflatex/xelatex (whatever is specified) multiple times
  ;; automatically to resolve the cross-references.
  (setq org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (add-to-list 'org-latex-classes
               '("elegantpaper"
                 "\\documentclass[lang=cn]{elegantpaper}
                   [NO-DEFAULT-PACKAGES]
                   [PACKAGES]
                   [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; 如果 org 设置了#+LATEX_HEADER: \usepackage{minted} 的话 则支持代码高亮和代码格式化
  (setq org-latex-listings 'minted)
  ;; Windows 加了这句的话 org 里 C-c C-x C-l 无法预览公式
  ;; 为了兼容性就不加这句 然后 org 里加上 #+LATEX_HEADER: \usepackage{minted} 即可
  ;; (add-to-list 'org-latex-packages-alist '("minted"))

  ;; 解决 org 文件里面执行  org-latex-preview 生成的 .tex 格式不对的问题
  ;; 通过 log 可以看到错误信息 ! Package minted Error: You must invoke LaTeX with the -shell-escape flag
  )

;; (use-package prism
;;   :straight (prism :type git :host github :repo "alphapapa/prism.el"))

;; (use-package color-identifiers-mode
;;   :straight t)
;; (use-package meow
;;   :straight t
;;   :hook (after-init . meow-global-mode)
;;   :demand t
;;   :custom-face
;;   (meow-normal-indicator ((t (:inherit (font-lock-function-name-face bold :inverse-video t)))))
;;   (meow-insert-indicator ((t (:inherit (font-lock-keyword-face bold :inverse-video t)))))
;;   (meow-keypad-indicator ((t (:inherit (font-lock-builtin-face bold :inverse-video t)))))
;;   (meow-beacon-indicator ((t (:inherit (font-lock-type-face bold :inverse-video t)))))
;;   (meow-motion-indicator ((t (:inherit (font-lock-doc-face bold :inverse-video t)))))
;;   :config
;;   (setq-default meow-replace-state-name-list '((normal . "N")
;;                                                (motion . "M")
;;                                                (keypad . "K")
;;                                                (insert . "I")
;;                                                (beacon . "B")))
;;
;;   ;; [motion]
;;   (meow-motion-overwrite-define-key
;;    '("j" . meow-next)
;;    '("k" . meow-prev)
;;    '("<escape>" . ignore))
;;
;;   ;; [leader]
;;   (meow-leader-define-key
;;    ;; SPC j/k will run the original command in MOTION state.
;;    '("j" . "H-j")
;;    '("k" . "H-k")
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet))
;;
;;   ;; [normal]
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("p" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("<escape>" . ignore))
;;
;;   (dolist
;;       (state
;;        '((telega-root-mode . motion)
;;          (telega-chat-mode . normal)
;;          (View-mode . normal)
;;          ;; (compilation-mode . normal)
;;          (blink-search-mode . insert)
;;          (rcirc-mode . normal)
;;          (comint-mode . normal)         ; IELM
;;          (fundamental-mode . normal)
;;          (message-mode . normal)
;;          (emacs-lisp-mode . normal)
;;          (eshell-mode . insert)
;;          (shell-mode . insert)
;;          (term-mode . insert)
;;          (vterm-mode . insert)
;;          (help-mode . normal)
;;          (vundo-mode . motion)))
;;     (add-to-list 'meow-mode-state-list state))
;;  )

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
