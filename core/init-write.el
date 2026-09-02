;;; -*- lexical-binding: t -*-

(use-package visual-fill-column
  :ensure t
  :hook ((text-mode eww-mode) . thy/center-text)
  :preface
  (defun thy/center-text ()
    "Center text in the current buffer with `visual-fill-column'."
    (interactive)
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))

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
  (ispell-personal-dictionary
   (expand-file-name "ispell/.pws" user-emacs-directory))
  (ispell-program-name (executable-find "aspell"))
  :config
  (setq ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir"))
  (setq ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir")))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map
        ("C-c C-b" . markdown-insert-bold)
        ("C-c C-e" . thy/markdown-export-pdf)
   :map gfm-view-mode-map
        ("C-c C-e" . thy/markdown-export-pdf))
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
  (defconst thy/markdown-pdf-stylesheet
    (no-littering-expand-etc-file-name "markdown-pdf/github.css")
    "GitHub-style CSS used when exporting Markdown to PDF.")

  (defconst thy/markdown-pdf-math-filter
    (no-littering-expand-etc-file-name "markdown-pdf/math-to-svg.lua")
    "Pandoc filter used to render Markdown math as local SVG images.")

  (defun thy/markdown-export-pdf ()
    "Export the current Markdown file to a GitHub-style PDF."
    (interactive)
    (unless buffer-file-name
      (user-error "Save this Markdown buffer before exporting it"))
    (dolist (program '("pandoc" "weasyprint" "xelatex" "pdf2svg"))
      (unless (executable-find program)
        (user-error "Markdown PDF export requires `%s'" program)))
    (dolist (file (list thy/markdown-pdf-stylesheet
                        thy/markdown-pdf-math-filter))
      (unless (file-readable-p file)
        (user-error "Markdown PDF resource is missing: %s" file)))
    (save-buffer)
    (let* ((source (file-truename buffer-file-name))
           (default-output (concat (file-name-sans-extension source) ".pdf"))
           (output
            (expand-file-name
             (read-file-name "Export PDF to: "
                             (file-name-directory source)
                             default-output nil
                             (file-name-nondirectory default-output))))
           (output (if (file-name-extension output)
                       output
                     (concat output ".pdf")))
           (log-buffer
            (get-buffer-create
             (format "*Markdown PDF: %s*" (file-name-nondirectory source))))
           (running (get-buffer-process log-buffer)))
      (unless (string-equal (downcase (or (file-name-extension output) "")) "pdf")
        (user-error "PDF output must use the .pdf extension"))
      (unless (file-directory-p (file-name-directory output))
        (user-error "Output directory does not exist: %s"
                    (file-name-directory output)))
      (when (and (file-exists-p output)
                 (not (y-or-n-p (format "Overwrite %s? "
                                        (abbreviate-file-name output)))))
        (user-error "PDF export cancelled"))
      (when (process-live-p running)
        (user-error "A PDF export is already running for this file"))
      (with-current-buffer log-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Exporting %s\n\n" source))))
      (make-process
       :name "markdown-pdf-export"
       :buffer log-buffer
       :command
       (list "pandoc" source
             "--from=gfm"
             "--to=html5"
             "--standalone"
             "--embed-resources"
             (concat "--resource-path=" (file-name-directory source))
             (concat "--css=" thy/markdown-pdf-stylesheet)
             (concat "--lua-filter=" thy/markdown-pdf-math-filter)
             "--syntax-highlighting=pygments"
             "--pdf-engine=weasyprint"
             (concat "--metadata=pagetitle:"
                     (file-name-base source))
             "--output" output)
       :coding 'utf-8-unix
       :noquery t
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (if (zerop (process-exit-status process))
               (let ((pdf-buffer (find-file-noselect output)))
                 (with-current-buffer pdf-buffer
                   (unless (verify-visited-file-modtime pdf-buffer)
                     (revert-buffer t t)))
                 (display-buffer pdf-buffer)
                 (message "Exported Markdown PDF: %s" output))
             (display-buffer (process-buffer process))
             (message "Markdown PDF export failed; see %s"
                      (buffer-name (process-buffer process)))))))))

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

  (defun thy/markdown-ts-align-wide-table (function &rest arguments)
    "Use display widths when FUNCTION aligns a table with wide characters."
    (let* ((at-table (markdown-ts-at-table-p nil t))
           (table (cdr at-table))
           (wide-character-p
            (and table
                 (save-excursion
                   (goto-char (treesit-node-start table))
                   (catch 'wide-character
                     (while (< (point) (treesit-node-end table))
                       (when (> (or (char-width (char-after)) 1) 1)
                         (throw 'wide-character t))
                       (forward-char))
                     nil)))))
      (if wide-character-p
          (progn
            (require 'markdown-mode)
            (markdown-table-align))
        (apply function arguments))))
  :custom-face
  (markdown-code-face ((t (:inherit nil))))
  (markdown-pre-face ((t (:inherit org-code))))
  (markdown-inline-code-face ((t (:inherit markdown-pre-face :extend nil))))
  (markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
  (markdown-table-face ((t (:inherit org-table))))
  :config
  (dolist (mapping '(("verilog" . verilog-mode)
                     ("c" . c-mode)
                     ("c++" . c++-mode)
                     ("cpp" . c++-mode)
                     ("sh" . shell-script-mode)
                     ("shell" . shell-script-mode)
                     ("bash" . shell-script-mode)))
    (add-to-list 'markdown-code-lang-modes mapping)))

(use-package markdown-ts-appear
  :vc (markdown-ts-appear
       :url "https://github.com/Thysrael/markdown-ts-appear"
       :rev :newest)
  :hook (markdown-ts-mode . markdown-ts-appear-mode)
  :custom
  (markdown-ts-appear-trigger 'evil-insert))

(use-package markdown-ts-mode
  :ensure nil
  :hook ((markdown-ts-mode . visual-line-mode)
         (markdown-ts-mode . thy/markdown-ts-editing-setup)
         (markdown-ts-mode . thy/markdown-ts-yank-media-setup))
  :bind
  (:map markdown-ts-mode-map
        ("C-c C-b" . thy/markdown-ts-insert-bold)
        ("C-c C-e" . thy/markdown-export-pdf)
        ("RET" . thy/markdown-ts-newline)
        ("<return>" . thy/markdown-ts-newline)
        ("<kp-enter>" . thy/markdown-ts-newline)
   :map markdown-ts-view-mode-map
        ("C-c C-e" . thy/markdown-export-pdf))
  :preface
  (defvar thy/markdown-image-directory-history nil
    "History of directories used for yanked Markdown images.")

  (defvar thy/markdown-image-basename-history nil
    "History of basenames used for yanked Markdown images.")

  (defvar-local thy/markdown-image-default-directory nil
    "Default image directory, relative to the current Markdown file.")

  (defconst thy/markdown-list-item-regexp
    "^\\([ \t]*\\(?:>[ \t]*\\)*\\)\\([-+*]\\|\\([0-9]+\\)\\([.)]\\)\\)[ \t]+\\(.*\\)$"
    "Regexp matching a plain Markdown list item.")

  (put 'thy/markdown-image-default-directory 'safe-local-variable
       #'string-or-null-p)

  (defun thy/markdown-ts-yank-image (mimetype data)
    "Save image DATA of MIMETYPE and insert a Markdown image link."
    (unless buffer-file-name
      (user-error "Save the Markdown buffer before yanking an image"))
    (let* ((base-directory (file-name-directory buffer-file-name))
           (default-directory-name
            (or thy/markdown-image-default-directory
                (file-name-base buffer-file-name)))
           (directory-name
            (read-string "Image directory: " default-directory-name
                         'thy/markdown-image-directory-history))
           (directory (expand-file-name directory-name base-directory))
           (extension
            (symbol-name (mailcap-mime-type-to-extension mimetype)))
           (basename
            (read-string "Image basename: " nil
                         'thy/markdown-image-basename-history))
           (filename (concat basename "." extension))
           (path (expand-file-name filename directory))
           (relative-path (file-relative-name path base-directory)))
      (when (or (string-empty-p basename)
                (member basename '("." ".."))
                (not (equal basename (file-name-nondirectory basename))))
        (user-error "Image basename must be a non-empty file basename"))
      (when (file-exists-p path)
        (user-error "Image already exists: %s" path))
      (make-directory directory t)
      (let ((coding-system-for-write 'emacs-internal))
        (with-temp-file path
          (insert data)))
      (insert "!["
              (string-replace "]" "\\]" (string-replace "\\" "\\\\" basename))
              "]" "("
              (replace-regexp-in-string
               "[()]" (lambda (match) (concat "\\" match))
               (url-encode-url relative-path))
              ")")))

  (defun thy/markdown-ts-yank-media-setup ()
    "Register the Markdown clipboard image handler."
    (require 'mailcap)
    (require 'url-util)
    (require 'yank-media)
    (yank-media-handler "image/.*" #'thy/markdown-ts-yank-image))

  (defun thy/markdown-ts-code-languages ()
    "Return known language names for Markdown fenced code blocks."
    (let (languages)
      (dolist (mapping (append (and (boundp 'markdown-ts-code-block-modes)
                                    markdown-ts-code-block-modes)
                               (and (boundp 'markdown-code-lang-modes)
                                    markdown-code-lang-modes)))
        (push (format "%s" (car mapping)) languages))
      (dolist (mapping auto-mode-alist)
        (when (and (symbolp (cdr mapping))
                   (string-suffix-p "-ts-mode" (symbol-name (cdr mapping))))
          (push (string-remove-suffix "-ts-mode" (symbol-name (cdr mapping)))
                languages)))
      (dolist (mapping major-mode-remap-alist)
        (dolist (mode (list (car mapping) (cdr mapping)))
          (when (symbolp mode)
            (push (string-remove-suffix
                   "-ts" (string-remove-suffix "-mode" (symbol-name mode)))
                  languages))))
      (sort (delete-dups languages) #'string<)))

  (defun thy/markdown-ts-refontify-fence (beg end _old-length)
    "Refontify from BEG after editing a Markdown code fence through END."
    (let ((line-beg (save-excursion
                      (goto-char beg)
                      (line-beginning-position)))
          (line-end (save-excursion
                      (goto-char end)
                      (line-end-position))))
      (when (save-excursion
              (goto-char line-beg)
              (re-search-forward
               "^[ \t]*\\(?:`\\{3,\\}\\|~\\{3,\\}\\)" line-end t))
        (font-lock-flush line-beg (point-max))
        (font-lock-ensure line-beg (point-max)))))

  (defun thy/markdown-ts-expand-code-fence ()
    "Prompt for a language and complete a newly typed code fence."
    (when (and (eq last-command-event ?`)
               (eolp)
               (equal (buffer-substring-no-properties
                       (line-beginning-position) (point))
                      "```"))
      (delete-region (line-beginning-position) (point))
      (condition-case nil
          (let ((language
                 (completing-read "Language: "
                                  (thy/markdown-ts-code-languages)
                                  nil nil nil
                                  'markdown-ts-language-history)))
            (insert "```" language "\n\n```")
            (forward-line -1))
        (quit (insert "```")))))

  (defun thy/markdown-ts-editing-setup ()
    "Enable personal Markdown editing helpers in the current buffer."
    (add-hook 'after-change-functions #'thy/markdown-ts-refontify-fence nil t)
    (add-hook 'post-self-insert-hook #'thy/markdown-ts-expand-code-fence nil t))

  (defun thy/markdown-ts-newline ()
    "Continue a plain ordered or unordered Markdown list."
    (interactive)
    (font-lock-ensure (line-beginning-position) (line-end-position))
    (if (or (markdown-ts-at-code-block-p)
            (not (save-excursion
                   (goto-char (line-beginning-position))
                   (re-search-forward thy/markdown-list-item-regexp
                                      (line-end-position) t))))
        (markdown-ts-newline)
      (let ((prefix (match-string-no-properties 1))
            (marker (match-string-no-properties 2))
            (number (match-string-no-properties 3))
            (delimiter (match-string-no-properties 4))
            (content (match-string-no-properties 5)))
        (if (string-empty-p (string-trim content))
            (delete-region (line-beginning-position) (line-end-position))
          (newline)
          (insert prefix
                  (if number
                      (concat (number-to-string (1+ (string-to-number number)))
                              delimiter)
                    marker)
                  " ")))))
  :custom-face
  (markdown-ts-heading-1 ((t (:inherit org-level-1 :height 1.5))))
  (markdown-ts-heading-2 ((t (:inherit org-level-2 :height 1.35))))
  (markdown-ts-heading-3 ((t (:inherit org-level-3 :height 1.2))))
  (markdown-ts-heading-4 ((t (:inherit org-level-4 :height 1.05))))
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
  (advice-add 'markdown-ts-table-align-table :around
              #'thy/markdown-ts-align-wide-table))

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
    (prettify-symbols-mode 1))

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
         ("\\.gnuplot\\'" . gnuplot-mode)
         ("\\.plt\\'" . gnuplot-mode)))

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
  :hook (TeX-mode . thy/latex-prettify-symbols)
  :custom
  (TeX-auto-save nil)
  (TeX-electric-sub-and-superscript t)
  (TeX-engine 'xetex)
  (TeX-parse-self t)
  (TeX-save-query nil)
  :preface
  (defun thy/latex-prettify-symbols ()
    "Add custom prettified LaTeX symbols to the current buffer."
    (push '("\\lnot" . ?¬) prettify-symbols-alist)
    (prettify-symbols-mode 1))
  :custom-face
  (font-latex-sedate-face ((t (:foreground "#ff5555" :weight bold))))
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' -shell-escape %t" TeX-run-TeX nil t))
  ;; AUCTeX provides this viewer when PDF Tools is installed.
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))

(use-package reftex
  :ensure nil
  :hook (LaTeX-mode . thy/latex-reftex-setup)
  :custom
  (reftex-plug-into-AUCTeX t)
  :preface
  (defvar thy/latex-citation-cache (make-hash-table :test #'equal)
    "Project bibliography cache keyed by project root.")

  (defun thy/latex-project-root ()
    "Return the current project root or `default-directory'."
    (expand-file-name
     (if-let ((project (project-current nil)))
         (project-root project)
       default-directory)))

  (defun thy/latex-bibliography-signature (files)
    "Return a modification signature for bibliography FILES."
    (mapcar (lambda (file)
              (let ((attributes (file-attributes file)))
                (list file
                      (and attributes (file-attribute-size attributes))
                      (and attributes
                           (file-attribute-modification-time attributes)))))
            files))

  (defun thy/latex-project-citation-keys ()
    "Return cached citation keys from bibliography files in the project."
    (let* ((root (thy/latex-project-root))
           (cached (gethash root thy/latex-citation-cache))
           (files (or (plist-get cached :files)
                      (let ((project (project-current nil root)))
                        (if project
                            (seq-filter
                             (lambda (file) (string-suffix-p ".bib" file t))
                             (project-files project))
                          (directory-files root t "\\.bib\\'")))))
           (signature (thy/latex-bibliography-signature files)))
      (if (equal signature (plist-get cached :signature))
          (plist-get cached :keys)
        (let (keys)
          (require 'bibtex)
          (dolist (file files)
            (when (file-readable-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (bibtex-mode)
                (bibtex-map-entries
                 (lambda (key _beg _end)
                   (when key (push key keys)))))))
          (setq keys (sort (delete-dups keys) #'string<))
          (puthash root (list :files files :signature signature :keys keys)
                   thy/latex-citation-cache)
          keys))))

  (defun thy/latex-project-cite-completion-at-point ()
    "Complete citation keys from project bibliography files."
    (when-let* ((macro (car-safe (LaTeX-what-macro)))
                ((string-match-p
                  "\\`[[:alpha:]@]*cite[[:alpha:]@*]*\\'" macro))
                (open (nth 1 (syntax-ppss)))
                ((eq (char-after open) ?{)))
      (let ((beg (save-excursion
                   (if (search-backward "," (1+ open) t)
                       (forward-char)
                     (goto-char (1+ open)))
                   (skip-chars-forward " \t")
                   (point)))
            (end (save-excursion
                   (skip-chars-forward "^,}")
                   (point))))
        (list beg end (thy/latex-project-citation-keys) :exclusive 'no))))

  (defun thy/latex-reftex-setup ()
    "Enable RefTeX and project citation completion."
    (turn-on-reftex)
    (add-hook 'completion-at-point-functions
              #'thy/latex-project-cite-completion-at-point -20 t)))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode))

(use-package bibtex
  :ensure nil
  :bind
  (:map bibtex-mode-map
        ("C-c C-f" . bibtex-reformat)))
