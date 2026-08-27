;; -*- lexical-binding: t; -*-

(use-package eww
  :ensure nil
  :preface
  (defun thy/toggle-eww ()
    "Switch to an existing EWW buffer, or prompt for a URL."
    (interactive)
    (if-let* ((buffer (get-buffer "*eww*")))
        (switch-to-buffer buffer)
      (eww (read-string "Enter URL: "))))
  :hook
  (eww-mode . thy/setup-eww-buffer)
  :bind
  ("C-c r e" . thy/toggle-eww)
  :custom
  (eww-retrieve-command '("readable"))
  (shr-max-image-proportion 0.6)
  :config
  (defun thy/setup-eww-buffer ()
    "Apply buffer-local reading settings for EWW."
    (setq-local fill-column 140)
    (setq-local line-spacing 0.15)
    (setq-local scroll-margin 2)
    (setq-local truncate-lines nil)))

(use-package image-mode
  :ensure nil
  :bind
  (:map image-mode-map
         ("=" . image-increase-size)
         ("-" . image-decrease-size))
  :custom
  (image-auto-resize 'fit-window)
  (image-auto-resize-on-window-resize 0.1)
  :config
  (with-eval-after-load 'evil
    (evil-define-key '(normal motion) image-mode-map
      (kbd "=") #'image-increase-size
      (kbd "-") #'image-decrease-size)))

(use-package csv-mode
  :ensure t
  :commands csv-mode)

(use-package pdf-tools
  :ensure t
  :defer t
  :preface
  (defconst thy/pdf-view-scroll-lines 3
    "Number of lines to scroll a PDF vertically per step.")

  (defconst thy/pdf-view-horizontal-scroll-columns 6
    "Number of columns to pan a PDF horizontally per step.")

  (defconst thy/pdf-view-pinch-resize-factor 1.1
    "Scale factor used for each touchpad pinch zoom step.")

  (defvar-local thy/pdf-view-pinch-start-scale nil
    "PDF scale at the start of the current pinch gesture.")

  (defvar-local thy/pdf-view-pinch-step 0
    "Zoom step applied during the current PDF pinch gesture.")

  (defun thy/setup-pdf-view-buffer ()
    "Apply buffer-local touchpad settings for PDF viewing."
    (setq-local mouse-wheel-tilt-scroll t
                mouse-wheel-flip-direction t
                mouse-wheel-scroll-amount
                (cons thy/pdf-view-horizontal-scroll-columns
                      (cdr mouse-wheel-scroll-amount))
                mouse-wheel-scroll-amount-horizontal
                thy/pdf-view-horizontal-scroll-columns))

  (defun thy/pdf-view-scroll-forward (&optional count)
    "Scroll forward by COUNT vertical PDF steps."
    (interactive "p")
    (let ((lines (* thy/pdf-view-scroll-lines (or count 1))))
      (if pdf-view-roll-minor-mode
          (pdf-roll-scroll-forward lines)
        (pdf-view-next-line-or-next-page lines))))

  (defun thy/pdf-view-scroll-backward (&optional count)
    "Scroll backward by COUNT vertical PDF steps."
    (interactive "p")
    (let ((lines (* thy/pdf-view-scroll-lines (or count 1))))
      (if pdf-view-roll-minor-mode
          (pdf-roll-scroll-backward lines)
        (pdf-view-previous-line-or-previous-page lines))))

  (defun thy/pdf-view-pan-left (&optional count)
    "Pan the PDF left by COUNT horizontal steps."
    (interactive "p")
    (image-backward-hscroll
     (* thy/pdf-view-horizontal-scroll-columns (or count 1))))

  (defun thy/pdf-view-pan-right (&optional count)
    "Pan the PDF right by COUNT horizontal steps."
    (interactive "p")
    (image-forward-hscroll
     (* thy/pdf-view-horizontal-scroll-columns (or count 1))))

  (defun thy/pdf-view-copy-page ()
    "Copy all text from the current PDF page."
    (interactive)
    (pdf-view-mark-whole-page)
    (pdf-view-kill-ring-save))

  (defun thy/pdf-view-pinch (event)
    "Zoom the PDF according to macOS pinch EVENT."
    (interactive "e")
    (unless (eq (event-basic-type event) 'pinch)
      (error "`thy/pdf-view-pinch' bound to bad event type"))
    (let ((window (posn-window (nth 1 event)))
          (dx (nth 2 event))
          (dy (nth 3 event))
          (scale (nth 4 event))
          (angle (nth 5 event)))
      (when (window-live-p window)
        (with-selected-window window
          (when (and (zerop dx) (zerop dy) (zerop angle))
            (setq thy/pdf-view-pinch-start-scale
                  (/ (float (car (pdf-view-image-size nil window)))
                     (car (pdf-cache-pagesize
                           (pdf-view-current-page window))))
                  thy/pdf-view-pinch-step 0))
          (when thy/pdf-view-pinch-start-scale
            (let ((step
                   (round (log scale thy/pdf-view-pinch-resize-factor))))
              (unless (= thy/pdf-view-pinch-step step)
                (setq thy/pdf-view-pinch-step step
                      pdf-view-display-size
                      (* thy/pdf-view-pinch-start-scale
                         (expt thy/pdf-view-pinch-resize-factor step)))
                (pdf-view-redisplay window))))))))
  :hook ((pdf-view-mode . thy/setup-pdf-view-buffer)
         (pdf-view-mode . pdf-view-roll-minor-mode)
         (pdf-view-mode . auto-revert-mode))
  :bind
  (:map pdf-view-mode-map
        ([pinch] . thy/pdf-view-pinch))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-resize-factor 1.2)
  :init
  (pdf-loader-install t)
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'pdf-view-mode 'motion)
    (evil-define-key* '(normal motion) pdf-view-mode-map
      (kbd "h") #'thy/pdf-view-pan-left
      (kbd "j") #'thy/pdf-view-scroll-forward
      (kbd "k") #'thy/pdf-view-scroll-backward
      (kbd "l") #'thy/pdf-view-pan-right
      (kbd "gg") #'pdf-view-first-page
      (kbd "G") #'pdf-view-last-page

      (kbd "/") #'isearch-forward
      (kbd "n") #'isearch-repeat-forward
      (kbd "N") #'isearch-repeat-backward

      (kbd "+") #'pdf-view-enlarge
      (kbd "=") #'pdf-view-enlarge
      (kbd "-") #'pdf-view-shrink

      (kbd "y") #'pdf-view-kill-ring-save
      (kbd "Y") #'thy/pdf-view-copy-page
      (kbd "M-w") #'pdf-view-kill-ring-save
      (kbd "M-c") #'pdf-view-kill-ring-save ; Command is Meta on macOS.

      ;; Evil's default mouse drag cannot select text rendered by PDF Tools.
      [down-mouse-1] #'pdf-view-mouse-set-region
      [M-down-mouse-1] #'pdf-view-mouse-set-region-rectangle
      [C-down-mouse-1] #'pdf-view-mouse-extend-region

      (kbd "q") #'quit-window)))

(use-package doc-view
  :ensure nil
  :preface
  (defvar-local thy/office-preview-source-file nil
    "Office file from which the current read-only preview was generated.")

  (defvar-local thy/xlsx-preview-files nil
    "CSV files generated from the current spreadsheet.")

  (defvar-local thy/doc-view-fitting-page nil
    "Non-nil while fitting the current DocView page to its window.")

  (defun thy/doc-view-fit-page (&rest _)
    "Fit the current DocView page within the selected window."
    (when (and (derived-mode-p 'doc-view-mode)
               (not thy/doc-view-fitting-page)
               (ignore-errors (image-get-display-property)))
      (let ((thy/doc-view-fitting-page t))
        (doc-view-fit-page-to-window))))

  (defun thy/doc-view-fit-frame-windows (frame)
    "Fit DocView pages displayed in windows on FRAME."
    (dolist (window (window-list frame 'no-minibuffer))
      (with-selected-window window
        (when (derived-mode-p 'doc-view-mode)
          (thy/doc-view-fit-page)))))

  (defun thy/office-preview-cache-directory (source)
    "Return the preview cache directory for Office file SOURCE."
    (let ((directory
           (no-littering-expand-var-file-name
            (file-name-concat
             "office-preview"
             (secure-hash 'sha256 (file-truename source))))))
      (make-directory directory t)
      directory))

  (defun thy/office-preview-run (program &rest args)
    "Run PROGRAM with ARGS, signaling an error when conversion fails."
    (unless (executable-find program)
      (user-error "Office preview requires `%s'" program))
    (with-temp-buffer
      (let ((status (apply #'call-process program nil t nil args)))
        (unless (and (integerp status) (zerop status))
          (error "%s conversion failed: %s"
                 program (string-trim (buffer-string)))))))

  (defun thy/office-preview-fresh-p (source outputs)
    "Return non-nil when OUTPUTS exist and are newer than SOURCE."
    (and outputs
         (cl-every (lambda (output)
                     (and (file-exists-p output)
                          (not (file-newer-than-file-p source output))))
                   outputs)))

  (defun thy/docx-preview-file (source)
    "Return a cached Markdown conversion of DOCX file SOURCE."
    (let* ((directory (thy/office-preview-cache-directory source))
           (output (file-name-concat directory "preview.md")))
      (unless (thy/office-preview-fresh-p source (list output))
        (let ((default-directory directory))
          (thy/office-preview-run
           "pandoc" source "--from=docx" "--to=gfm" "--wrap=none"
           "--extract-media=." (concat "--output=" output))))
      output))

  (defun thy/xlsx-preview-generate-files (source)
    "Return CSV files generated from every worksheet in SOURCE."
    (let* ((directory (thy/office-preview-cache-directory source))
           (files (directory-files directory t "\\.csv\\'")))
      (unless (thy/office-preview-fresh-p source files)
        (mapc #'delete-file files)
        (let ((profile (make-temp-file "libreoffice-preview-" t)))
          (unwind-protect
              (thy/office-preview-run
               "soffice"
               (concat "-env:UserInstallation=file://" profile)
               "--headless" "--convert-to"
               "csv:Text - txt - csv (StarCalc):44,34,76,1,,0,false,true,true,false,false,-1"
               "--outdir" directory source)
            (delete-directory profile t)))
        (setq files (directory-files directory t "\\.csv\\'")))
      (or files (error "LibreOffice produced no CSV preview for %s" source))))

  (defun thy/office-preview-set-read-only (source directory)
    "Mark the current buffer as a read-only preview of SOURCE in DIRECTORY."
    (setq-local thy/office-preview-source-file source)
    (setq-local default-directory (file-name-as-directory directory))
    (setq-local revert-buffer-function #'thy/office-preview-revert)
    (setq-local buffer-read-only t)
    (auto-save-mode -1)
    (set-buffer-modified-p nil))

  (defun thy/docx-preview-mode ()
    "Display the current DOCX file as read-only Markdown."
    (let* ((source buffer-file-name)
           (preview (thy/docx-preview-file source))
           (mode (alist-get 'markdown-mode major-mode-remap-alist
                            'markdown-mode)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents preview))
      (funcall mode)
      (thy/office-preview-set-read-only source (file-name-directory preview))))

  (defun thy/xlsx-preview-select-file (files)
    "Prompt for one worksheet CSV from FILES when necessary."
    (if (cdr files)
        (let* ((names (mapcar #'file-name-base files))
               (name (completing-read "Worksheet: " names nil t)))
          (nth (seq-position names name #'equal) files))
      (car files)))

  (defun thy/xlsx-preview-display (source files)
    "Display one of the worksheet CSV FILES generated from SOURCE."
    (let ((preview (thy/xlsx-preview-select-file files)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents preview))
      (csv-mode)
      (setq-local thy/xlsx-preview-files files)
      (thy/office-preview-set-read-only source (file-name-directory preview))))

  (defun thy/xlsx-preview-mode ()
    "Display the current XLSX file as a read-only CSV worksheet."
    (let ((source buffer-file-name))
      (thy/xlsx-preview-display
       source (thy/xlsx-preview-generate-files source))))

  (defun thy/xlsx-preview-select-sheet ()
    "Select another worksheet in the current XLSX preview."
    (interactive)
    (unless thy/office-preview-source-file
      (user-error "This is not an XLSX preview"))
    (thy/xlsx-preview-display thy/office-preview-source-file
                              thy/xlsx-preview-files))

  (defun thy/office-preview-revert (&optional _ignore-auto _noconfirm)
    "Regenerate the current Office preview from its source file."
    (let ((source thy/office-preview-source-file)
          (inhibit-read-only t))
      (unless source
        (user-error "This buffer has no Office preview source"))
      (pcase (downcase (file-name-extension source))
        ("docx" (thy/docx-preview-mode))
        ("xlsx" (thy/xlsx-preview-mode))
        (_ (user-error "Unsupported Office preview type")))))

  :mode (("\\.docx\\'" . thy/docx-preview-mode)
         ("\\.xlsx\\'" . thy/xlsx-preview-mode)
         ("\\.pptx\\'" . doc-view-mode-maybe))
  :bind
  (:map doc-view-mode-map
        ("=" . doc-view-enlarge)
        ("-" . doc-view-shrink))
  :custom
  (doc-view-cache-directory (no-littering-expand-var-file-name "doc-view/"))
  (doc-view-resolution 200)
  :config
  (advice-add #'doc-view-goto-page :after #'thy/doc-view-fit-page)
  (add-hook 'window-size-change-functions #'thy/doc-view-fit-frame-windows)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'doc-view-mode 'motion)
    (evil-define-key '(normal motion) doc-view-mode-map
      (kbd "j") #'doc-view-next-page
      (kbd "k") #'doc-view-previous-page
      (kbd "=") #'doc-view-enlarge
      (kbd "-") #'doc-view-shrink)))
