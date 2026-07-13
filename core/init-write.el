;;; -*- lexical-binding: t -*-

(use-package visual-fill-column
  :ensure t
  :hook ((markdown-mode markdown-ts-mode markdown-view-mode markdown-ts-view-mode org-mode eww-mode gfm-mode gfm-view-mode LaTeX-mode) . thy/center-text)
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
  :bind
  (:map markdown-mode-map
        ("C-c C-b" . markdown-insert-bold))
  :hook ((gfm-mode markdown-ts-mode markdown-ts-view-mode) . thy/set-prose-line-spacing)
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
  (custom-theme-set-faces
   'user
   '(markdown-code-face ((t (:inherit nil))))
   '(markdown-header-face-1 ((t (:inherit org-level-1))))
   '(markdown-header-face-2 ((t (:inherit org-level-2))))
   '(markdown-header-face-3 ((t (:inherit org-level-3))))
   '(markdown-header-face-4 ((t (:inherit org-level-4))))
   '(markdown-pre-face ((t (:inherit org-code))))
   '(markdown-inline-code-face ((t (:inherit markdown-pre-face :extend nil))))
   '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
   '(markdown-table-face ((t (:inherit org-table)))))

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
  :hook ((markdown-ts-mode . visual-line-mode)
         (markdown-ts-mode . thy/markdown-ts-appear-mode))
  :bind
  (:map markdown-ts-mode-map
         ("C-c C-b" . thy/markdown-ts-insert-bold))
  :preface
  (defvar-local thy/markdown-ts-appear-region nil
    "Markers delimiting the region whose Markdown markup is visible.")

  (defvar-local thy/markdown-ts-appear-previous-hide-markup nil
    "Value of `markdown-ts-hide-markup' before appear mode was enabled.")

  (defun thy/markdown-ts-appear--restore ()
    "Restore hidden markup in the previously revealed region."
    (when-let* ((region thy/markdown-ts-appear-region)
                (beg (marker-position (car region)))
                (end (marker-position (cdr region))))
      (font-lock-flush beg end)
      (font-lock-ensure beg end)
      (set-marker (car region) nil)
      (set-marker (cdr region) nil)
      (setq thy/markdown-ts-appear-region nil)))

  (defun thy/markdown-ts-appear-bounds ()
    "Return the line or fenced code block bounds at point."
    (font-lock-ensure (line-beginning-position)
                      (min (point-max) (line-beginning-position 2)))
    (let ((overlay
           (seq-find (lambda (candidate)
                       (overlay-get candidate 'markdown-ts-code-block))
                     (overlays-at (point)))))
      (if overlay
          (cons (save-excursion
                  (goto-char (overlay-start overlay))
                  (line-beginning-position 0))
                (save-excursion
                  (goto-char (overlay-end overlay))
                  (min (point-max) (line-beginning-position 2))))
        (cons (line-beginning-position)
              (min (point-max) (line-beginning-position 2))))))

  (defun thy/markdown-ts-appear-at-point ()
    "Reveal hidden markup on the current line or fenced code block."
    (pcase-let ((`(,beg . ,end) (thy/markdown-ts-appear-bounds)))
      (unless (and thy/markdown-ts-appear-region
                   (= beg (marker-position (car thy/markdown-ts-appear-region))))
        (thy/markdown-ts-appear--restore)
        (font-lock-ensure beg end)
        (with-silent-modifications
          (let ((pos beg))
            (while (< pos end)
              (let ((next (next-single-property-change
                           pos 'invisible nil end)))
                (when (eq (get-text-property pos 'invisible)
                          'markdown-ts--markup)
                  (remove-text-properties pos next '(invisible nil)))
                (setq pos next)))))
        (setq thy/markdown-ts-appear-region
              (cons (copy-marker beg) (copy-marker end t))))))

  (defun thy/markdown-ts-appear-start ()
    "Reveal Markdown markup while Evil is in insert state."
    (add-hook 'post-command-hook #'thy/markdown-ts-appear-at-point nil t)
    (thy/markdown-ts-appear-at-point))

  (defun thy/markdown-ts-appear-stop ()
    "Hide Markdown markup after leaving Evil insert state."
    (remove-hook 'post-command-hook #'thy/markdown-ts-appear-at-point t)
    (when (and (fboundp 'markdown-ts-at-table-p)
               (ignore-errors (markdown-ts-at-table-p nil t)))
      (ignore-errors (markdown-ts-table-align-table)))
    (thy/markdown-ts-appear--restore))

  (defun thy/markdown-ts-fontify-delimiter (function node &rest arguments)
    "Call FUNCTION while preserving code fences and styling quote markers."
    (let* ((type (treesit-node-type node))
           (hide-markup-p markdown-ts-hide-markup)
           (quote-marker-p
            (and (member type '("block_quote_marker" "block_continuation"))
                 (eq (char-after (treesit-node-start node)) ?>)))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup
                 (not (or quote-marker-p
                          (member type '("fenced_code_block_delimiter"
                                         "info_string")))))))
      (apply function node arguments)
      (when (equal type "fenced_code_block_delimiter")
        (let ((face (if hide-markup-p
                        'markdown-ts-code-block-markup-hidden
                      'markdown-ts-code-block)))
          (save-excursion
            (goto-char (treesit-node-start node))
            (add-face-text-property
             (line-beginning-position)
             (min (point-max) (1+ (line-end-position)))
             face t))))))

  (define-minor-mode thy/markdown-ts-appear-mode
    "Reveal nearby Markdown markup while Evil is in insert state."
    :lighter nil
    (if thy/markdown-ts-appear-mode
        (progn
          (setq thy/markdown-ts-appear-previous-hide-markup
                markdown-ts-hide-markup)
          (unless markdown-ts-hide-markup
            (markdown-ts-toggle-hide-markup))
          (add-hook 'evil-insert-state-entry-hook
                    #'thy/markdown-ts-appear-start nil t)
          (add-hook 'evil-insert-state-exit-hook
                    #'thy/markdown-ts-appear-stop nil t)
          (when (eq (bound-and-true-p evil-state) 'insert)
            (thy/markdown-ts-appear-start)))
      (remove-hook 'evil-insert-state-entry-hook
                   #'thy/markdown-ts-appear-start t)
      (remove-hook 'evil-insert-state-exit-hook
                   #'thy/markdown-ts-appear-stop t)
      (thy/markdown-ts-appear-stop)
      (unless thy/markdown-ts-appear-previous-hide-markup
        (when markdown-ts-hide-markup
          (markdown-ts-toggle-hide-markup)))))
  :custom-face
  (markdown-ts-heading-1 ((t (:inherit org-level-1))))
  (markdown-ts-heading-2 ((t (:inherit org-level-2))))
  (markdown-ts-heading-3 ((t (:inherit org-level-3))))
  (markdown-ts-heading-4 ((t (:inherit org-level-4))))
  (markdown-ts-code-block
   ((((background light)) (:inherit fixed-pitch :background "#f3f3f3" :extend t))
    (((background dark)) (:inherit fixed-pitch :background "#30323b" :extend t))))
  (markdown-ts-code-block-markup-hidden
   ((((background light)) (:inherit fixed-pitch :background "#f3f3f3" :extend t))
    (((background dark)) (:inherit fixed-pitch :background "#30323b" :extend t))))
  (markdown-ts-code-span
   ((t (:inherit (fixed-pitch font-lock-constant-face) :extend nil))))
  (markdown-ts-table
   ((((background light)) (:inherit fixed-pitch :background "#f7f7f7" :extend t))
    (((background dark)) (:inherit fixed-pitch :background "#2b2d35" :extend t))))
  (markdown-ts-table-header
   ((t (:inherit markdown-ts-table :weight bold))))
  (markdown-ts-table-cell ((t (:inherit markdown-ts-table))))
  (markdown-ts-table-delimiter-cell
   ((t (:inherit (markdown-ts-table shadow)))))
  :custom
  (markdown-ts-table-auto-align t)
  :config
  (advice-add 'markdown-ts--fontify-delimiter :around
              #'thy/markdown-ts-fontify-delimiter))

(use-package org
  :ensure nil
  :init
  (setq org-element-cache-persistent nil)
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :extend nil :weight bold))))
  (org-level-2 ((t (:inherit outline-2 :extend nil :weight bold))))
  (org-level-3 ((t (:inherit outline-3 :extend nil :weight bold))))
  (org-level-4 ((t (:inherit outline-4 :extend nil :weight bold))))
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

  (defun thy/org-find-or-create-heading-path (headings)
    "Move to HEADINGS in the current Org buffer, creating missing entries."
    (let (path parent-marker)
      (dolist (heading headings)
        (setq path (append path (list heading)))
        (let ((marker (condition-case nil
                          (org-find-olp path t)
                        (error nil))))
          (unless marker
            (if parent-marker
                (progn
                  (goto-char parent-marker)
                  (org-end-of-subtree t t))
              (goto-char (point-max)))
            (unless (bolp)
              (insert "\n"))
            (let ((start (point)))
              (insert (make-string (length path) ?*) " " heading "\n")
              (setq marker (copy-marker start))))
          (setq parent-marker marker)))
      (when parent-marker
        (goto-char parent-marker))
      parent-marker))

  (defun thy/org-archive-subtree-hierarchical ()
    "Archive the current subtree while preserving its parent hierarchy."
    (interactive)
    (require 'org-archive)
    (org-back-to-heading t)
    (let* ((source-buffer (current-buffer))
           (source-start (copy-marker (point)))
           (source-end (copy-marker
                        (save-excursion
                          (org-end-of-subtree t t)
                          (point))))
           (subtree (buffer-substring-no-properties source-start source-end))
           (parent-headings (org-get-outline-path))
           (location (or (org-entry-get nil "ARCHIVE" 'inherit)
                          org-archive-location))
           (archive-location (org-archive--compute-location location))
           (archive-file (car archive-location))
           (archive-heading (cdr archive-location))
           (archive-buffer
             (if (string-empty-p archive-file)
                 source-buffer
               (find-file-noselect archive-file)))
           (archive-root
            (unless (string-empty-p archive-heading)
              (unless (string-match-p "\\`\\*+\\s-+" archive-heading)
                (user-error "Unsupported hierarchical archive target: %s"
                            archive-heading))
              (with-temp-buffer
                (org-mode)
                (insert archive-heading)
                (goto-char (point-min))
                (org-get-heading t t t t))))
           (target-path (append (and archive-root (list archive-root))
                                parent-headings)))
      (cl-labels
          ((archive-subtree
            ()
            (with-current-buffer archive-buffer
              (org-mode)
              (if (thy/org-find-or-create-heading-path target-path)
                  (org-end-of-subtree t t)
                (goto-char (point-max)))
              (unless (bolp)
                (insert "\n"))
              (insert subtree)
              (unless (string-suffix-p "\n" subtree)
                (insert "\n"))
              (unless (eq source-buffer archive-buffer)
                (save-buffer)))
            (with-current-buffer source-buffer
              (goto-char source-start)
              (org-cut-subtree))))
        (if (eq source-buffer archive-buffer)
            (atomic-change-group
              (archive-subtree))
          (archive-subtree)))
      (message "Subtree archived in %s" (abbreviate-file-name archive-file))))
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

(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package gnuplot
  :ensure t
  :mode (("\\.gp\\'" . gnuplot-mode)
         ("\\.gnuplot\\'" . gnuplot-mode)))

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
  :preface
  (defun thy/latex-prettify-symbols ()
    "Add custom prettified LaTeX symbols to the current buffer."
    (push '("\\lnot" . ?¬) prettify-symbols-alist)
    (prettify-symbols-mode 1))
  :config
  (custom-theme-set-faces
   'user
   '(font-latex-sedate-face ((t (:foreground "#ff5555" :weight bold)))))
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

(use-package bibtex
  :ensure nil
  :bind
  (:map bibtex-mode-map
        ("C-c C-f" . bibtex-reformat)))
