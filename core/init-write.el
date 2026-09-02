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

(use-package mathjax
  :ensure t
  :commands (mathjax-available-p mathjax-render))

(use-package markdown-ts-mode
  :ensure nil
  :hook ((markdown-ts-mode . visual-line-mode)
         (markdown-ts-mode . thy/markdown-ts-appear-mode)
         (markdown-ts-mode . thy/markdown-ts-math-preview-setup)
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
  (defvar-local thy/markdown-ts-appear-region nil
    "Markers delimiting the semantic Markdown source currently visible.")

  (defun thy/markdown-ts-appear-region-visible-p (beg end)
    "Return non-nil when BEG through END overlaps visible Markdown source."
    (when-let* ((region thy/markdown-ts-appear-region)
                (visible-beg (marker-position (car region)))
                (visible-end (marker-position (cdr region))))
      (and (< beg visible-end) (> end visible-beg))))

  (defvar-local thy/markdown-ts-appear-previous-hide-markup nil
    "Value of `markdown-ts-hide-markup' before appear mode was enabled.")

  (defvar-local thy/markdown-ts-appear-managed-line-height-p nil
    "Non-nil when appear mode added `line-height' to managed properties.")

  (defvar thy/markdown-ts-math-cache (make-hash-table :test #'equal)
    "MathJax results keyed by formula text and display style.")

  (defvar thy/markdown-ts-math-pending (make-hash-table :test #'equal)
    "Pending MathJax requests keyed by formula text and display style.")

  (defconst thy/markdown-ts-math-cache-miss
    (make-symbol "markdown-ts-math-cache-miss")
    "Sentinel used for missing MathJax cache entries.")

  (defconst thy/markdown-ts-math-timeout 10
    "Seconds to wait for a MathJax rendering result.")

  (defvar thy/markdown-ts-image-icon nil
    "Cached image icon used in rendered Markdown links.")

  (defvar thy/markdown-ts-link-icon nil
    "Cached link icon used in rendered Markdown links.")

  (defvar thy/markdown-ts-wikilink-icon nil
    "Cached Wiki link icon used in rendered Markdown links.")

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

  (defun thy/markdown-ts-math-node-data (node)
    "Return (BEG END MATH DISPLAY-P) for a LaTeX block NODE."
    (let (opening closing)
      (dotimes (index (treesit-node-child-count node))
        (let ((child (treesit-node-child node index)))
          (when (equal (treesit-node-type child) "latex_span_delimiter")
            (unless opening
              (setq opening child))
            (setq closing child))))
      (when (and opening closing
                 (< (treesit-node-start opening) (treesit-node-start closing)))
        (let ((opener (treesit-node-text opening t)))
          (list (treesit-node-start node)
                (treesit-node-end node)
                (buffer-substring-no-properties
                 (treesit-node-end opening) (treesit-node-start closing))
                (and (member opener '("$$" "\\[")) t))))))

  (defun thy/markdown-ts-math-node-at (position)
    "Return the valid Markdown LaTeX block at POSITION, if any."
    (when-let* ((node (treesit-node-at position 'markdown-inline))
                (math-node (treesit-parent-until
                            node "\\`latex_block\\'" t))
                ((<= (treesit-node-start math-node) position))
                ((< position (treesit-node-end math-node)))
                ((markdown-ts--latex-block-valid-p math-node)))
      math-node))

  (defun thy/markdown-ts-math-clear (beg end)
    "Remove a rendered formula between BEG and END."
    (with-silent-modifications
      (remove-text-properties
       beg end '(display nil thy/markdown-ts-math-state nil))))

  (defun thy/markdown-ts-math-state (beg end)
    "Return the uniform math preview state between BEG and END."
    (let ((state (get-text-property beg 'thy/markdown-ts-math-state)))
      (when (and state
                 (= (next-single-property-change
                     beg 'thy/markdown-ts-math-state nil end)
                    end))
        state)))

  (defun thy/markdown-ts-math-image (svg)
    "Create an image from MathJax SVG with a suitable baseline."
    (let* ((height
            (and (string-match "height=\"\\([-.0-9]+\\)" svg)
                 (string-to-number (match-string 1 svg))))
           (vertical-align
            (and (string-match "vertical-align: \\([-.0-9]+\\)" svg)
                 (string-to-number (match-string 1 svg))))
           (ascent (if (and height vertical-align (> height 0))
                       (round (* 100 (/ (+ height vertical-align) height)))
                     100)))
      (svg-image svg :ascent (max 0 (min 100 ascent)))))

  (defun thy/markdown-ts-math-display-result (request data)
    "Display MathJax DATA for a still-valid rendering REQUEST."
    (pcase-let ((`(,buffer ,beg-marker ,end-marker ,source ,key) request))
      (unwind-protect
          (when (and (buffer-live-p buffer)
                     (marker-position beg-marker)
                     (marker-position end-marker))
            (with-current-buffer buffer
              (save-restriction
                (widen)
                (let ((beg (marker-position beg-marker))
                      (end (marker-position end-marker)))
                  (when (and thy/markdown-ts-math-preview-mode
                             (< beg end)
                             (equal source
                                    (buffer-substring-no-properties beg end))
                             (not (thy/markdown-ts-appear-region-visible-p
                                   beg end)))
                    (when-let* ((node (thy/markdown-ts-math-node-at beg))
                                (node-data
                                 (thy/markdown-ts-math-node-data node)))
                      (pcase-let ((`(,node-beg ,node-end ,math ,display-p)
                                    node-data))
                        (when (and (= beg node-beg) (= end node-end)
                                   (equal key (cons display-p math)))
                          (thy/markdown-ts-math-clear beg end)
                          (with-silent-modifications
                            (if-let* ((svg (alist-get 'svg data)))
                                (progn
                                  (put-text-property
                                   beg end 'display
                                   (thy/markdown-ts-math-image svg))
                                  (put-text-property
                                   beg end 'thy/markdown-ts-math-state
                                   (list 'rendered key)))
                              (unless (alist-get 'transient data)
                                (put-text-property
                                 beg end 'thy/markdown-ts-math-state
                                 (list 'error key)))))))))))))
        (set-marker beg-marker nil)
        (set-marker end-marker nil))))

  (defun thy/markdown-ts-math-finish-render (key data)
    "Cache MathJax DATA for KEY and complete its waiting requests."
    (unless (alist-get 'transient data)
      (when (>= (hash-table-count thy/markdown-ts-math-cache) 512)
        (clrhash thy/markdown-ts-math-cache))
      (puthash key data thy/markdown-ts-math-cache))
    (when-let* ((pending (gethash key thy/markdown-ts-math-pending)))
      (remhash key thy/markdown-ts-math-pending)
      (cancel-timer (car pending))
      (maphash
       (lambda (_ requests)
         (dolist (request requests)
           (condition-case error
               (thy/markdown-ts-math-display-result request data)
             (error
              (message "Markdown math preview failed: %s"
                       (error-message-string error))))))
       (cdr pending))))

  (defun thy/markdown-ts-math-render-timeout (key)
    "Release requests waiting too long for MathJax KEY."
    (thy/markdown-ts-math-finish-render
     key '((error . "MathJax rendering timed out") (transient . t))))

  (defun thy/markdown-ts-math-request (beg end math display-p)
    "Render MATH asynchronously for the region from BEG to END."
    (let* ((key (cons display-p math))
           (state (thy/markdown-ts-math-state beg end))
           (cached (gethash key thy/markdown-ts-math-cache
                            thy/markdown-ts-math-cache-miss)))
      (unless (and (consp state) (equal (cadr state) key))
        (thy/markdown-ts-math-clear beg end)
        (with-silent-modifications
          (put-text-property beg end 'thy/markdown-ts-math-state
                             (list 'pending key)))
        (let* ((source (buffer-substring-no-properties beg end))
               (request-key (list (current-buffer) beg end source))
               (request (list (current-buffer)
                              (copy-marker beg t)
                              (copy-marker end)
                              source key)))
          (if (not (eq cached thy/markdown-ts-math-cache-miss))
              (thy/markdown-ts-math-display-result request cached)
            (let ((pending (gethash key thy/markdown-ts-math-pending)))
              (if pending
                  (let* ((requests (cdr pending))
                         (bucket (gethash request-key requests)))
                    (unless (seq-some
                             (lambda (waiting)
                               (and (equal (marker-position (cadr waiting)) beg)
                                    (equal (marker-position (caddr waiting)) end)
                                    (equal (nth 3 waiting) source)))
                             bucket)
                      (puthash request-key (cons request bucket) requests)))
                (let ((requests (make-hash-table :test #'equal)))
                  (puthash request-key (list request) requests)
                  (setq pending
                        (cons
                         (run-at-time
                          thy/markdown-ts-math-timeout nil
                          #'thy/markdown-ts-math-render-timeout key)
                         requests)))
                (puthash key pending thy/markdown-ts-math-pending)
                (condition-case error
                    (mathjax-render
                     (lambda (data)
                       (thy/markdown-ts-math-finish-render key data))
                     math :options (list :display display-p))
                  (error
                   (thy/markdown-ts-math-finish-render
                    key `((error . ,(error-message-string error))
                          (transient . t))))))))))))

  (defun thy/markdown-ts-math-preview-node (node)
    "Render the valid Markdown LaTeX block NODE when it is not being edited."
    (when (and thy/markdown-ts-math-preview-mode
               (markdown-ts--latex-block-valid-p node))
      (when-let* ((data (thy/markdown-ts-math-node-data node)))
        (pcase-let ((`(,beg ,end ,math ,display-p) data))
          (if (thy/markdown-ts-appear-region-visible-p beg end)
              (thy/markdown-ts-math-clear beg end)
            (unless (markdown-ts--outline-invisible-p beg)
              (thy/markdown-ts-math-request beg end math display-p)))))))

  (defun thy/markdown-ts-fontify-math (function node &rest arguments)
    "Call FUNCTION and asynchronously render LaTeX block NODE."
    (let ((markdown-ts-hide-markup
           (and markdown-ts-hide-markup
                (not (thy/markdown-ts-appear-node-visible-p node)))))
      (apply function node arguments))
    (thy/markdown-ts-math-preview-node node))

  (defun thy/markdown-ts-math-preview-window (window)
    "Enable math preview when WINDOW shows this buffer graphically."
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer))
               (display-graphic-p (window-frame window))
               (not thy/markdown-ts-math-preview-mode))
      (thy/markdown-ts-math-preview-mode 1)))

  (defun thy/markdown-ts-math-graphic-window ()
    "Return a graphical window displaying the current buffer."
    (seq-find
     (lambda (window)
       (display-graphic-p (window-frame window)))
     (get-buffer-window-list nil nil t)))

  (defun thy/markdown-ts-math-filter-copied-text (text)
    "Remove math preview properties from copied Markdown TEXT."
    (remove-text-properties
     0 (length text) '(display nil thy/markdown-ts-math-state nil) text)
    text)

  (defun thy/markdown-ts-math-preview-setup ()
    "Enable math preview now or when this buffer reaches a graphical frame."
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'thy/markdown-ts-math-filter-copied-text)
    (add-hook 'window-buffer-change-functions
              #'thy/markdown-ts-math-preview-window nil t)
    (when-let* ((window (thy/markdown-ts-math-graphic-window)))
      (thy/markdown-ts-math-preview-window window)))

  (define-minor-mode thy/markdown-ts-math-preview-mode
    "Render Markdown LaTeX fragments asynchronously with MathJax."
    :lighter nil
    (if thy/markdown-ts-math-preview-mode
        (if (and (or (display-graphic-p)
                     (thy/markdown-ts-math-graphic-window))
                 (image-type-available-p 'svg)
                 (require 'mathjax nil t)
                 (mathjax-available-p))
            (progn
              (remove-hook 'window-buffer-change-functions
                           #'thy/markdown-ts-math-preview-window t)
              (add-to-list 'font-lock-extra-managed-props
                           'thy/markdown-ts-math-state)
              (font-lock-flush))
          (setq thy/markdown-ts-math-preview-mode nil))
      (save-restriction
        (widen)
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let ((next (next-single-property-change
                         pos 'thy/markdown-ts-math-state nil (point-max))))
              (when (get-text-property pos 'thy/markdown-ts-math-state)
                (thy/markdown-ts-math-clear pos next))
              (setq pos next))))
        (setq font-lock-extra-managed-props
              (delq 'thy/markdown-ts-math-state
                    font-lock-extra-managed-props))
        (font-lock-flush))))

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

  (defun thy/markdown-ts-icon (type)
    "Return the cached Markdown icon for TYPE."
    (let* ((variable (pcase type
                       ('image 'thy/markdown-ts-image-icon)
                       ('wikilink 'thy/markdown-ts-wikilink-icon)
                       (_ 'thy/markdown-ts-link-icon)))
           (cached (symbol-value variable)))
      (or cached
          (set variable
               (cond
                ((eq type 'wikilink)
                 (propertize "◆" 'face 'markdown-ts-link))
                ((require 'nerd-icons nil t)
                 (nerd-icons-octicon
                  (if (eq type 'image) "nf-oct-image" "nf-oct-link")
                  :face 'markdown-ts-link))
                ((eq type 'image) "[image]")
                (t "[link]"))))))

  (defun thy/markdown-ts-appear--restore ()
    "Restore hidden markup in the previously revealed region."
    (when-let* ((region thy/markdown-ts-appear-region)
                (beg (marker-position (car region)))
                (end (marker-position (cdr region))))
      (set-marker (car region) nil)
      (set-marker (cdr region) nil)
      (setq thy/markdown-ts-appear-region nil)
      (save-restriction
        (widen)
        (font-lock-flush beg end)
        (font-lock-ensure beg end))))

  (defun thy/markdown-ts-appear-node-visible-p (node)
    "Return non-nil when NODE overlaps visible semantic Markdown source."
    (thy/markdown-ts-appear-region-visible-p
     (treesit-node-start node) (treesit-node-end node)))

  (defun thy/markdown-ts-appear-bounds ()
    "Return source bounds for the smallest rendered element at point."
    (font-lock-ensure (line-beginning-position)
                      (min (point-max) (line-beginning-position 2)))
    (let* ((pos (point))
           (line-beg (line-beginning-position))
           (line-end (line-end-position))
           (contains-p
            (lambda (node)
              (let ((beg (treesit-node-start node))
                    (end (treesit-node-end node)))
                (and (<= beg pos)
                     (or (< pos end)
                         (and (= pos end) (> pos beg)
                              (not (eq (char-before pos) ?\n))))))))
           inline-node)
      (let ((node (treesit-node-at pos 'markdown-inline)))
        (while (and node (not inline-node))
          (let ((type (treesit-node-type node)))
            (when (and
                   (funcall contains-p node)
                   (member type
                           '("emphasis" "strong_emphasis" "strikethrough"
                             "code_span" "inline_link" "full_reference_link"
                             "collapsed_reference_link" "shortcut_link"
                             "image" "uri_autolink" "email_autolink"
                             "entity_reference" "numeric_character_reference"
                             "backslash_escape" "hard_line_break"
                             "latex_block"))
                   (or (not (equal type "latex_block"))
                       (markdown-ts--latex-block-valid-p node)))
              (setq inline-node node)))
          (setq node (and node (treesit-node-parent node)))))
      ;; The inline grammar represents `~~text~~' as nested strikethroughs.
      (when (and inline-node
                 (equal (treesit-node-type inline-node) "strikethrough"))
        (let ((parent (treesit-node-parent inline-node)))
          (while (and parent
                      (equal (treesit-node-type parent) "strikethrough"))
            (setq inline-node parent
                  parent (treesit-node-parent parent)))))
      (if inline-node
          (let ((beg (treesit-node-start inline-node))
                (end (treesit-node-end inline-node)))
            ;; Wiki links are parsed as a shortcut link inside extra brackets.
            (when (and (equal (treesit-node-type inline-node) "shortcut_link")
                       (> beg (point-min)) (< end (point-max))
                       (eq (char-before beg) ?\[)
                       (eq (char-after end) ?\]))
              (setq beg (1- beg)
                    end (1+ end)))
            (cons beg end))
        (let (structural-node)
          (let ((node (treesit-node-at pos 'markdown)))
            (while (and node (not structural-node))
              (when (and
                     (funcall contains-p node)
                     (member (treesit-node-type node)
                             '("atx_heading" "setext_heading" "list_item"
                               "task_list_marker_unchecked"
                               "task_list_marker_checked"
                               "pipe_table_header" "pipe_table_row"
                               "pipe_table_delimiter_row"
                               "fenced_code_block" "thematic_break"
                               "link_reference_definition")))
                (setq structural-node node))
              (setq node (and node (treesit-node-parent node)))))
          (or
           (pcase (and structural-node
                       (treesit-node-type structural-node))
             ("atx_heading"
              (when-let* ((marker (treesit-node-child structural-node 0 'named)))
                (cons (treesit-node-start marker)
                      (save-excursion
                        (goto-char (treesit-node-end marker))
                        (skip-chars-forward " \t" line-end)
                        (point)))))
             ("setext_heading"
              (when-let* ((underline
                           (treesit-search-subtree
                            structural-node "\\`setext_h[12]_underline\\'")))
                (cons (treesit-node-start underline)
                      (treesit-node-end underline))))
             ("list_item"
              (when-let* ((marker
                           (treesit-node-child structural-node 0 'named))
                          ((string-prefix-p
                            "list_marker_" (treesit-node-type marker)))
                          ((= line-beg
                              (save-excursion
                                (goto-char (treesit-node-start marker))
                                (line-beginning-position)))))
                (cons (treesit-node-start marker)
                      (treesit-node-end marker))))
             ((or "task_list_marker_unchecked" "task_list_marker_checked"
                  "pipe_table_header" "pipe_table_row"
                  "pipe_table_delimiter_row" "fenced_code_block"
                  "thematic_break" "link_reference_definition")
              (cons (treesit-node-start structural-node)
                    (treesit-node-end structural-node))))
           ;; Keep malformed markup stable while its closing syntax is typed.
           (save-excursion
             (goto-char pos)
             (skip-chars-backward "^ \t\n" line-beg)
             (let ((beg (point)))
               (goto-char pos)
               (skip-chars-forward "^ \t\n" line-end)
               (let ((end (point)))
                 (when (and (< beg end)
                            (or (memq (char-after beg)
                                      '(?* ?_ ?~ ?` ?$ ?< ?\\ ?\[ ?!))
                                (string-match-p
                                 "\\[" (buffer-substring-no-properties
                                         beg end))))
                   (cons beg end))))))))))

  (defun thy/markdown-ts-appear-at-point ()
    "Reveal source for the rendered Markdown element at point."
    (save-restriction
      (widen)
      (let* ((bounds (thy/markdown-ts-appear-bounds))
             (beg (car-safe bounds))
             (end (cdr-safe bounds))
             (region thy/markdown-ts-appear-region)
             (old-beg (and region (marker-position (car region))))
             (old-end (and region (marker-position (cdr region)))))
        (unless (if bounds
                    (and old-beg old-end (= beg old-beg) (= end old-end))
                  (null region))
          (thy/markdown-ts-appear--restore)
          (when bounds
            (setq thy/markdown-ts-appear-region
                  (cons (copy-marker beg) (copy-marker end t)))
            (font-lock-flush beg end)
            (font-lock-ensure beg end))))))

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

  (defun thy/markdown-ts-fontify-delimiter (function node &rest arguments)
    "Call FUNCTION while preserving code fences and styling quote markers."
    (let* ((type (treesit-node-type node))
           (hide-markup-p markdown-ts-hide-markup)
           (quote-marker-p
            (and (member type '("block_quote_marker" "block_continuation"))
                 (eq (char-after (treesit-node-start node)) ?>)))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup
                 (not (thy/markdown-ts-appear-node-visible-p node))
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

  (defun thy/markdown-ts-fontify-image (function node &rest arguments)
    "Call FUNCTION and render image NODE with an icon and useful label."
    (let* ((beg (treesit-node-start node))
           (end (treesit-node-end node))
           (description
            (treesit-search-subtree node "\\`image_description\\'"))
           (visible-p
            (and (thy/markdown-ts-appear-node-visible-p node)
                 (or (not description)
                     (thy/markdown-ts-appear-region-visible-p
                      beg (treesit-node-start description))
                     (thy/markdown-ts-appear-region-visible-p
                      (treesit-node-end description) end)))))
      (let ((markdown-ts-inline-images
             (and markdown-ts-inline-images (not visible-p))))
        (apply function node arguments))
      (dolist (overlay (overlays-in beg end))
        (when (overlay-get overlay 'thy/markdown-ts-image-label)
          (delete-overlay overlay)))
      (when (and markdown-ts-hide-markup
                  (not visible-p)
                  (not (markdown-ts--outline-invisible-p beg)))
        (let* ((destination
                (treesit-search-subtree node "\\`link_destination\\'"))
               (url (and destination (treesit-node-text destination t)))
               (icon (thy/markdown-ts-icon 'image))
               (overlay (make-overlay beg (min (1+ beg) end) nil t nil)))
          (with-silent-modifications
            (when (and (not description) destination)
              (remove-text-properties (treesit-node-start destination)
                                      (treesit-node-end destination)
                                      '(invisible nil)))
            (when url
              (markdown-ts--make-link-button beg end url)))
          (overlay-put overlay 'thy/markdown-ts-image-label t)
          (overlay-put overlay 'before-string (concat icon " "))
          (overlay-put overlay 'help-echo url)
          (overlay-put overlay 'mouse-face 'highlight)
          (overlay-put overlay 'evaporate t)))))

  (defun thy/markdown-ts-fontify-link (function node &rest arguments)
    "Call FUNCTION and prefix a non-image link NODE with an icon."
    (apply function node arguments)
    (let* ((parent (treesit-node-parent node))
           (parent-beg (treesit-node-start parent))
           (parent-end (treesit-node-end parent))
           (wikilink-p
            (and (equal (treesit-node-type parent) "shortcut_link")
                 (> parent-beg (point-min))
                 (< parent-end (point-max))
                 (eq (char-before parent-beg) ?\[)
                 (eq (char-after parent-end) ?\])))
           (beg (treesit-node-start node))
           (end (treesit-node-end node))
           (alias-beg
            (and wikilink-p
                 (save-excursion
                   (goto-char beg)
                   (search-forward "|" end t))))
           (icon-beg (or alias-beg beg))
           (visible-region thy/markdown-ts-appear-region)
           (visible-beg
            (and visible-region
                 (marker-position (car visible-region))))
           (visible-end
            (and visible-region
                 (marker-position (cdr visible-region))))
           (visible-p
            (or (thy/markdown-ts-appear-region-visible-p parent-beg beg)
                (thy/markdown-ts-appear-region-visible-p end parent-end))))
      (dolist (overlay (overlays-in beg end))
        (when (overlay-get overlay 'thy/markdown-ts-link-icon)
          (delete-overlay overlay)))
      (when (and markdown-ts-hide-markup
                  (not (equal (treesit-node-type parent) "image"))
                  (not visible-p))
        (when wikilink-p
          (with-silent-modifications
            (put-text-property (1- parent-beg) parent-beg
                               'invisible 'markdown-ts--markup)
            (put-text-property parent-end (1+ parent-end)
                               'invisible 'markdown-ts--markup)
            (when alias-beg
              (if (and visible-beg visible-end
                       (< visible-beg alias-beg) (> visible-end beg))
                  (let ((reveal-beg (max beg visible-beg))
                        (reveal-end (min alias-beg visible-end)))
                    (when (< beg reveal-beg)
                      (put-text-property beg reveal-beg
                                         'invisible 'markdown-ts--markup))
                    (when (< reveal-end alias-beg)
                      (put-text-property reveal-end alias-beg
                                         'invisible 'markdown-ts--markup)))
                (put-text-property beg alias-beg
                                   'invisible 'markdown-ts--markup)))))
        (let ((overlay (make-overlay icon-beg (min (1+ icon-beg) end)
                                     nil t nil)))
          (overlay-put overlay 'thy/markdown-ts-link-icon t)
          (overlay-put overlay 'before-string
                       (concat (thy/markdown-ts-icon
                                (if wikilink-p 'wikilink 'link))
                               " "))
          (overlay-put overlay 'evaporate t)))))

  (defun thy/markdown-ts-fontify-visible-markup (function node &rest arguments)
    "Call FUNCTION without rendering NODE over visible semantic source."
    (let* ((type (treesit-node-type node))
           (markup-node
            (pcase type
              ("atx_heading" (treesit-node-child node 0 'named))
              ("setext_heading"
               (treesit-search-subtree node "\\`setext_h[12]_underline\\'"))
              (_ node)))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup
                 (not (and markup-node
                           (thy/markdown-ts-appear-node-visible-p
                            markup-node))))))
      (apply function node arguments)))

  (defun thy/markdown-ts-fontify-atx-delimiter (function node &rest arguments)
    "Call FUNCTION and hide only the ATX marker and following blanks."
    (let ((hide-markup-p
           (and markdown-ts-hide-markup
                (not (thy/markdown-ts-appear-node-visible-p node)))))
      (let ((markdown-ts-hide-markup nil))
        (apply function node arguments))
      (when hide-markup-p
        (save-excursion
          (goto-char (treesit-node-end node))
          (skip-chars-forward " \t" (line-end-position))
          (put-text-property (treesit-node-start node) (point)
                             'invisible 'markdown-ts--markup)))))

  (define-minor-mode thy/markdown-ts-appear-mode
    "Reveal semantic Markdown source while Evil is in insert state."
    :lighter nil
    (if thy/markdown-ts-appear-mode
        (progn
          (setq thy/markdown-ts-appear-previous-hide-markup
                markdown-ts-hide-markup)
          (unless (memq 'line-height font-lock-extra-managed-props)
            (setq thy/markdown-ts-appear-managed-line-height-p t)
            (add-to-list 'font-lock-extra-managed-props 'line-height))
          (unless markdown-ts-hide-markup
            (markdown-ts-toggle-hide-markup))
          (add-hook 'evil-insert-state-entry-hook
                    #'thy/markdown-ts-appear-start nil t)
          (add-hook 'evil-insert-state-exit-hook
                    #'thy/markdown-ts-appear-stop nil t)
          (add-hook 'after-change-functions
                    #'thy/markdown-ts-refontify-fence nil t)
          (add-hook 'post-self-insert-hook
                    #'thy/markdown-ts-expand-code-fence nil t)
          (when (eq (bound-and-true-p evil-state) 'insert)
            (thy/markdown-ts-appear-start)))
      (remove-hook 'evil-insert-state-entry-hook
                   #'thy/markdown-ts-appear-start t)
      (remove-hook 'evil-insert-state-exit-hook
                   #'thy/markdown-ts-appear-stop t)
      (remove-hook 'after-change-functions
                   #'thy/markdown-ts-refontify-fence t)
      (remove-hook 'post-self-insert-hook
                   #'thy/markdown-ts-expand-code-fence t)
      (thy/markdown-ts-appear-stop)
      (unless thy/markdown-ts-appear-previous-hide-markup
        (when markdown-ts-hide-markup
          (markdown-ts-toggle-hide-markup)))
      (when thy/markdown-ts-appear-managed-line-height-p
        (setq font-lock-extra-managed-props
              (delq 'line-height font-lock-extra-managed-props))
        (setq thy/markdown-ts-appear-managed-line-height-p nil))))
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
  (advice-add 'markdown-ts--fontify-delimiter :around
               #'thy/markdown-ts-fontify-delimiter)
  (advice-add 'markdown-ts--fontify-atx-delimiter :around
               #'thy/markdown-ts-fontify-atx-delimiter)
  (dolist (function '(markdown-ts--fontify-atx-heading
                      markdown-ts--fontify-setext-heading
                      markdown-ts--fontify-link-destination
                      markdown-ts--fontify-link-ref-label
                      markdown-ts--fontify-link-ref-destination
                      markdown-ts--fontify-unordered-list-marker
                      markdown-ts--fontify-checkbox
                      markdown-ts--fontify-autolink
                      markdown-ts--fontify-backslash-escape
                      markdown-ts--fontify-entity
                      markdown-ts--fontify-hard-line-break
                      markdown-ts--fontify-thematic-break))
    (advice-add function :around #'thy/markdown-ts-fontify-visible-markup))
  (advice-add 'markdown-ts--fontify-link-node :around
              #'thy/markdown-ts-fontify-link)
  (advice-add 'markdown-ts--fontify-image :around
              #'thy/markdown-ts-fontify-image)
  (advice-add 'markdown-ts--fontify-latex-block :around
              #'thy/markdown-ts-fontify-math)
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
