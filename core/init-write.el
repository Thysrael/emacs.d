;;; -*- lexical-binding: t -*-

(use-package visual-fill-column
  :ensure t
  :hook ((markdown-mode markdown-ts-mode markdown-view-mode org-mode eww-mode gfm-mode gfm-view-mode LaTeX-mode) . thy/center-text)
  :preface
  (defun thy/center-text ()
    "Center text in the current buffer with `visual-fill-column'."
    (interactive)
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode)))

(use-package pangu-spacing
  :ensure t
  :hook ((eww-mode chatgpt-shell-mode) . pangu-spacing-mode))

(use-package ispell
  :ensure nil
  :if (executable-find "aspell")
  :unless thy/on-server
  :preface
  (defun thy/org-ispell-skip-region-alist ()
    "Skip Org source blocks, markup, and math regions during spell checking."
    (make-local-variable 'ispell-skip-region-alist)
    (dolist (pair '((org-property-drawer-re)
                    ("~" "~") ("=" "=")
                    ("^#\\+BEGIN_SRC" "^#\\+END_SRC")
                    ("\\\\(" "\\\\)") ("\\[" "\\]")
                    ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))

  (defun thy/markdown-ispell-skip-region-alist ()
    "Skip Markdown code, template, and math regions during spell checking."
    (make-local-variable 'ispell-skip-region-alist)
    (dolist (pair '(("`" "`")
                    ("^```" "^```")
                    ("{{" "}}")
                    ("\\\\(" "\\\\)") ("\\[" "\\]")
                    ("^\\\\begin{[^}]+}" "^\\\\end{[^}]+}")))
      (add-to-list 'ispell-skip-region-alist pair)))
  :hook ((org-mode . thy/org-ispell-skip-region-alist)
          ((markdown-mode markdown-ts-mode) . thy/markdown-ispell-skip-region-alist))
  :custom
  (ispell-dictionary "en_US")
  (ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  (ispell-program-name (executable-find "aspell"))
  :config
  (setq ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir"))
  (setq ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir"))
  (setq ispell-personal-dictionary (expand-file-name "ispell/.pws" user-emacs-directory)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom-face
  (markdown-code-face ((t (:inherit nil))))
  (markdown-header-face-1 ((t (:inherit org-level-1))))
  (markdown-header-face-2 ((t (:inherit org-level-2))))
  (markdown-header-face-3 ((t (:inherit org-level-3))))
  (markdown-header-face-4 ((t (:inherit org-level-4))))
  (markdown-pre-face ((t (:inherit org-code))))
  (markdown-inline-code-face ((t (:inherit markdown-pre-face :extend nil))))
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  (markdown-table-face ((t (:inherit org-table))))
  :bind
  (:map markdown-mode-map
        ("C-c C-v" . thy/toggle-markdown-mode)
        ("C-c C-b" . markdown-insert-bold))
  :hook ((gfm-mode markdown-ts-mode) . thy/set-prose-line-spacing)
  :custom
  (markdown-asymmetric-header t)
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-italic-underscore t)
  (markdown-nested-imenu-heading-index t)
  :preface
  (defun thy/set-prose-line-spacing ()
    "Use slightly looser line spacing in prose buffers."
    (setq line-spacing 0.25))

  (defun thy/toggle-markdown-mode ()
    "Toggle between the active Markdown editing and viewing modes."
    (interactive)
    (pcase major-mode
      ('markdown-ts-mode (markdown-ts-view-mode))
      ('markdown-ts-view-mode (markdown-ts-mode))
      ('gfm-mode (gfm-view-mode))
      (_ (gfm-mode))))

  (defun thy/markdown-ts-insert-bold ()
    "Insert or apply bold emphasis in `markdown-ts-mode'."
    (interactive)
    (markdown-ts-emphasize ?b))
  :config
  (dolist (mapping '(("verilog" . verilog-mode)
                     ("c" . c-mode)
                     ("c++" . c++-mode)
                     ("cpp" . c++-mode)
                     ("sh" . shell-script-mode)
                     ("shell" . shell-script-mode)
                     ("bash" . shell-script-mode)))
    (add-to-list 'markdown-code-lang-modes mapping)))

(use-package markdown-ts-mode
  :ensure nil
  :bind
  (:map markdown-ts-mode-map
        ("C-c C-v" . thy/toggle-markdown-mode)
        ("C-c C-b" . thy/markdown-ts-insert-bold))
  :custom-face
  (markdown-ts-heading-1 ((t (:inherit org-level-1))))
  (markdown-ts-heading-2 ((t (:inherit org-level-2))))
  (markdown-ts-heading-3 ((t (:inherit org-level-3))))
  (markdown-ts-heading-4 ((t (:inherit org-level-4))))
  (markdown-ts-code-block ((t (:inherit org-code))))
  (markdown-ts-code-span ((t (:inherit markdown-ts-code-block :extend nil))))
  (markdown-ts-delimiter ((t (:foreground "#616161" :height 0.9))))
  (markdown-ts-table ((t (:inherit org-table))))
  (markdown-ts-table-cell ((t (:inherit markdown-ts-table))))
  (markdown-ts-table-header ((t (:inherit markdown-ts-table)))))

(use-package org
  :ensure nil
  :init
  (setq org-element-cache-persistent nil)
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-level-2 ((t (:inherit outline-2 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-level-3 ((t (:inherit outline-3 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-level-4 ((t (:inherit outline-4 :extend nil :weight bold :family "Sarasa Mono SC"))))
  (org-table ((t (:family "Sarasa Mono SC"))))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-ellipsis "¶")
  (org-emphasis-alist '(("*" org-bold)
                        ("/" italic)
                        ("_" underline)
                        ("=" org-verbatim verbatim)
                        ("~" org-code verbatim)
                        ("+" (:strike-through t))))
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native))
  (org-image-actual-width (if (string-equal (system-name) "banana") '(1200) '(600)))
  (org-latex-create-formula-image-program 'dvisvgm)
  (org-link-descriptive t)
  (org-list-demote-modify-bullet '(("-" . "+")
                                   ("+" . "1.")
                                   ("1." . "1)")))
  (org-preview-latex-image-directory "/tmp/ltximg/")
  (org-pretty-entities t)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-startup-numerated t)
  (org-use-sub-superscripts "{}")
  (org-yank-dnd-method 'file-link)
  (org-yank-image-save-method "./img")
  :hook
  (org-mode . thy/set-prose-line-spacing)
  (org-mode . thy/disable-electric-indent)
  (org-mode . thy/org-icons)
  :bind
  (:map org-mode-map
        ("C-c C-q" . org-cut-subtree)
        ("C-c C-b" . org-emphasize)
        ("C-," . nil))
  :preface
  (defface org-bold
    '((t :foreground "#d2268b"
         :family "Sarasa Mono SC"
         :weight bold))
    "Face for org-mode bold."
    :group 'org-faces)

  (defun thy/disable-electric-indent ()
    "Disable electric indentation in the current buffer."
    (electric-indent-local-mode 0))

  (defun thy/org-icons ()
    "Beautify Org mode keywords."
    (setq prettify-symbols-alist
          '(("#+begin_src" . "󰗀")
            ("#+end_src" . "󰗀")
            ("#+begin_quote" . "󰝗")
            ("#+end_quote" . "󰉾")
            ("#+RESULTS" . "󰐪")
            ("SCHEDULED" . "󰸗")
            ("DEADLINE" . "󰃰")
            ("CLOCK" . "󰥔")
            (":CREATED:" . "󱓞")))
    (setq prettify-symbols-unprettify-at-point nil)
    (prettify-symbols-mode))
  :config
  (plist-put org-format-latex-options :scale 1.0)
  (push '("jupyter-python" . python) org-src-lang-modes))

(use-package org-appear
  :ensure t
  :hook ((org-mode . org-appear-mode)
         (org-mode . thy/org-add-appear-hook))
  :custom
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-delay 0.1)
  (org-appear-inside-latex t)
  (org-appear-trigger 'manual)
  :preface
  (defun thy/org-add-appear-hook ()
    "Toggle `org-appear' while entering and leaving Evil insert state."
    (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
    (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t)))

(use-package advance-words-count
  :vc (advance-words-count :url "https://github.com/Thysrael/advance-words-count.el" :rev "master")
  :commands advance-words-count)

(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package gnuplot
  :ensure t)

(use-package org-src
  :ensure nil
  :custom
  (org-babel-default-header-args '((:eval . "never-export")
                                   (:session . "none")
                                   (:results . "value verbatim output replace")
                                   (:exports . "both")
                                   (:cache . "no")
                                   (:noweb . "no")
                                   (:hlines . "no")
                                   (:tangle . "no")))
  (org-babel-load-languages '((python . t)
                              (C . t)
                              (emacs-lisp . t)
                              (shell . t)
                              (gnuplot . t))))

(use-package latex
  :ensure auctex
  :hook ((TeX-mode . prettify-symbols-mode)
         (TeX-mode . thy/latex-prettify-symbols))
  :custom
  (TeX-auto-save t)
  (TeX-electric-sub-and-superscript t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  :custom-face
  (font-latex-sedate-face ((t (:foreground "#ff5555" :weight bold))))
  :preface
  (defun thy/latex-prettify-symbols ()
    "Add custom prettified LaTeX symbols to the current buffer."
    (push '("\\lnot" . ?¬) prettify-symbols-alist)
    (prettify-symbols-mode 1))
  :config
  (setq-default TeX-engine 'xetex)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' -shell-escape %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
  (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode))

(use-package outline
  :ensure nil
  :hook (LaTeX-mode . outline-minor-mode)
  :bind
  (:map outline-minor-mode-map
        ("C-c C-o" . outline-cycle)
        ("C-c [" . nil)))

(use-package bibtex-mode
  :ensure nil
  :bind
  (:map bibtex-mode-map
        ("C-c C-f" . bibtex-reformat)))
