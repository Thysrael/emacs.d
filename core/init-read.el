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
  :vc (pdf-tools
       :url "https://codeberg.org/rahguzar/pdf-tools.git"
       :branch "upstream-child-frame-preview"
       :lisp-dir "lisp"
       :rev :newest)
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
          (let* ((window (selected-window))
                 (page (pdf-view-current-page window)))
            (pdf-roll-scroll-backward lines window)
            (unless (= page (pdf-view-current-page window))
              (pdf-roll-redisplay window)))
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
  ;; PDF Tools defines this map in the `pdf-view' subfeature, after `pdf-tools'.
  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "g") #'thy/pdf-view-revert))
  (with-eval-after-load 'pdf-isearch
    (define-key pdf-isearch-active-mode-map (kbd "<escape>") #'isearch-exit))
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'pdf-view-mode 'motion)
    (evil-define-key* '(normal motion) pdf-view-mode-map
      (kbd "h") #'thy/pdf-view-pan-left
      (kbd "j") #'thy/pdf-view-scroll-forward
      (kbd "k") #'thy/pdf-view-scroll-backward
      (kbd "l") #'thy/pdf-view-pan-right
      (kbd "gg") #'pdf-view-first-page
      (kbd "gr") #'thy/pdf-view-revert
      (kbd "G") #'pdf-view-last-page

      (kbd "s") #'isearch-forward
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

  (defvar-local thy/office-preview-process nil
    "LibreOffice process generating the current PDF preview.")

  (defvar-local thy/xlsx-preview-files nil
    "CSV files generated from the current spreadsheet.")

  (defvar-local thy/doc-view-fitting-page nil
    "Non-nil while fitting the current DocView page to its window.")

  (defun thy/doc-view-fit-page (&rest _)
    "Fit the current DocView page within the selected window."
    (let ((display (image-get-display-property)))
      (when (and (derived-mode-p 'doc-view-mode)
                 (not thy/doc-view-fitting-page)
                 (or (imagep display)
                     (and (listp display) (assq 'image display))))
        (let ((thy/doc-view-fitting-page t))
          (doc-view-fit-page-to-window)))))

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

  (defun thy/office-preview-file-signature (file)
    "Return the modification time and size of FILE."
    (when-let* ((attributes (file-attributes file 'string)))
      (list (file-attribute-modification-time attributes)
            (file-attribute-size attributes))))

  (defun thy/office-pdf-preview-cancel ()
    "Cancel the PDF conversion requested by the current buffer."
    (when (process-live-p thy/office-preview-process)
      (process-put thy/office-preview-process 'thy/cancelled t)
      (delete-process thy/office-preview-process)))

  (defun thy/office-pdf-preview-display (source output source-buffer)
    "Display cached PDF OUTPUT for Office SOURCE, replacing SOURCE-BUFFER."
    (let* ((existing (get-file-buffer output))
           (preview-buffer (find-file-noselect output)))
      (with-current-buffer preview-buffer
        (when (and existing (derived-mode-p 'pdf-view-mode))
          (pdf-view-revert-buffer :ignore-auto :noconfirm))
        (setq-local thy/office-preview-source-file source)
        (setq-local thy/office-preview-process nil)
        (setq-local default-directory (file-name-directory source))
        (add-hook 'kill-buffer-hook #'thy/office-pdf-preview-cancel nil t)
        (when (fboundp '+mode-line-update-project-crumb)
          (+mode-line-update-project-crumb))
        (rename-buffer
         (format "%s [PDF preview]" (file-name-nondirectory source)) t))
      (if (buffer-live-p source-buffer)
          (progn
            (dolist (window (get-buffer-window-list source-buffer nil t))
              (set-window-buffer window preview-buffer))
            (kill-buffer source-buffer))
        (display-buffer preview-buffer))))

  (defun thy/office-pdf-preview-finish
      (process source signature output generated workspace source-buffer)
    "Finish Office PDF PROCESS and display its OUTPUT for SOURCE.
SIGNATURE identifies SOURCE when conversion started.  GENERATED is
LibreOffice's output, WORKSPACE contains its temporary files, and
SOURCE-BUFFER is the buffer that requested the preview."
    (when (memq (process-status process) '(exit signal))
      (let ((retry nil))
        (unwind-protect
            (cond
             ((process-get process 'thy/cancelled))
             ((not (equal signature
                          (thy/office-preview-file-signature source)))
              (setq retry (and (buffer-live-p source-buffer)
                               (file-exists-p source)))
              (message "Office source changed; restarting PDF preview..."))
             ((and (zerop (process-exit-status process))
                   (file-exists-p generated))
              (rename-file generated output t)
              (if (and (buffer-live-p source-buffer)
                       (with-current-buffer source-buffer
                         (derived-mode-p 'pdf-view-mode)))
                  (with-current-buffer source-buffer
                    (pdf-view-revert-buffer :ignore-auto :noconfirm)
                    (setq-local thy/office-preview-source-file source)
                    (setq-local thy/office-preview-process nil)
                    (setq-local default-directory (file-name-directory source))
                    (add-hook 'kill-buffer-hook
                              #'thy/office-pdf-preview-cancel nil t)
                    (when (fboundp '+mode-line-update-project-crumb)
                      (+mode-line-update-project-crumb)))
                (thy/office-pdf-preview-display
                 source output source-buffer))
              (message "Generated PDF preview for %s"
                       (file-name-nondirectory source)))
             (t
              (when-let* ((buffer (process-buffer process)))
                (display-buffer buffer))
              (message "Unable to generate PDF preview for %s"
                       (file-name-nondirectory source))))
          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer
              (setq thy/office-preview-process nil)))
          (ignore-errors (delete-directory workspace t)))
        (when retry
          (run-at-time
           0 nil
           (lambda ()
             (when (buffer-live-p source-buffer)
               (thy/office-pdf-preview-start
                source source-buffer t))))))))

  (defun thy/office-pdf-preview-start (source source-buffer &optional force)
    "Display a cached PDF for Office SOURCE, converting it when needed.
SOURCE-BUFFER requested the preview.  With FORCE, regenerate the PDF."
    (let* ((directory (thy/office-preview-cache-directory source))
           (output (file-name-concat directory "preview.pdf")))
      (if (and (not force)
               (thy/office-preview-fresh-p source (list output)))
          (run-at-time
           0 nil
           (lambda ()
             (when (buffer-live-p source-buffer)
               (thy/office-pdf-preview-display
                source output source-buffer))))
        (unless (executable-find "soffice")
          (user-error "Office preview requires `soffice'"))
        (let* ((name
                (format "office-pdf-%s"
                        (substring (secure-hash 'sha256 source) 0 12)))
               (existing (get-process name)))
          (when (process-live-p existing)
            (user-error
             "A PDF preview is already being generated for this file"))
          (let* ((workspace (make-temp-file "office-pdf-preview-" t))
                 (profile
                  (if (seq-some
                       (lambda (process)
                         (and (process-live-p process)
                              (process-get process 'thy/office-pdf-preview)))
                       (process-list))
                      (file-name-concat workspace "profile")
                    (no-littering-expand-var-file-name
                     "office-preview/libreoffice-profile/")))
                 (staging (file-name-concat workspace "output"))
                 (generated
                  (file-name-concat
                   staging (concat (file-name-base source) ".pdf")))
                 (signature (thy/office-preview-file-signature source))
                 (log-buffer
                  (get-buffer-create
                   (format "*Office PDF: %s*"
                           (file-name-nondirectory source)))))
            (condition-case error-data
                (progn
                  (make-directory profile t)
                  (make-directory staging)
                  (with-current-buffer log-buffer
                    (let ((inhibit-read-only t))
                      (erase-buffer)))
                  (let ((process
                         (make-process
                          :name name
                          :buffer log-buffer
                          :command
                          (list
                           "soffice"
                           (concat "-env:UserInstallation=file://" profile)
                           "--headless" "--norestore" "--nologo"
                           "--nodefault" "--nofirststartwizard"
                           "--convert-to" "pdf"
                           "--outdir" staging source)
                          :connection-type 'pipe
                          :noquery t
                          :sentinel
                          (lambda (process _event)
                            (thy/office-pdf-preview-finish
                             process source signature output generated
                             workspace source-buffer)))))
                    (process-put process 'thy/office-pdf-preview t)
                    (when (buffer-live-p source-buffer)
                      (with-current-buffer source-buffer
                        (setq-local thy/office-preview-process process)))
                    (message "Generating PDF preview for %s..."
                             (file-name-nondirectory source))))
              (error
               (ignore-errors (delete-directory workspace t))
               (signal (car error-data) (cdr error-data)))))))))

  (define-derived-mode thy/office-pdf-preview-mode special-mode
    "Office-PDF"
    "Generate and display the current Office document with PDF Tools."
    (let ((source buffer-file-name))
      (unless source
        (user-error "This buffer is not visiting an Office document"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Generating PDF preview for %s...\n"
                        (file-name-nondirectory source))))
      (set-buffer-modified-p nil)
      (add-hook 'kill-buffer-hook #'thy/office-pdf-preview-cancel nil t)
      (thy/office-pdf-preview-start source (current-buffer))))

  (defun thy/office-pdf-preview-refresh ()
    "Regenerate the PDF preview from its Office source file."
    (interactive)
    (unless thy/office-preview-source-file
      (user-error "This PDF is not an Office preview"))
    (thy/office-pdf-preview-start
     thy/office-preview-source-file (current-buffer) t))

  (defun thy/pdf-view-revert ()
    "Regenerate an Office preview, or revert a regular PDF buffer."
    (interactive)
    (if thy/office-preview-source-file
        (thy/office-pdf-preview-refresh)
      (revert-buffer)))

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
        ("xlsx" (thy/xlsx-preview-mode))
        (_ (user-error "Unsupported Office preview type")))))

  :mode (("\\.\\(?:docx?\\|pptx?\\)\\'" . thy/office-pdf-preview-mode)
         ("\\.xlsx\\'" . thy/xlsx-preview-mode))
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
