;;; -*- lexical-binding: t -*-

(use-package dired
  :ensure nil
  :preface
  (defun thy/dired-create-file-or-directory (path)
    "Create PATH as a directory when it ends in slash, or as an empty file."
    (interactive
     (list (read-file-name "Create file or directory: " default-directory)))
    (let* ((directory-p (string-suffix-p "/" path))
           (target (expand-file-name
                    (if directory-p (directory-file-name path) path))))
      (when (or (file-exists-p target) (file-symlink-p target))
        (user-error "%s already exists" (abbreviate-file-name target)))
      (if directory-p
          (make-directory target t)
        (make-empty-file target t))
      (revert-buffer t t)
      (dired-goto-file target)
      (message "Created %s" (abbreviate-file-name target))))
  :bind
  (:map dired-mode-map
        ("C-c C-p" . wdired-change-to-wdired-mode)
        ("a" . thy/dired-create-file-or-directory)
        ("z" . dired-do-compress)
        ("Z" . dired-do-compress-to)
        ("W" . thy/dired-copy-files-to-clipboard))
  :custom
  (dired-compress-directory-default-suffix ".zip")
  (dired-compress-file-default-suffix ".zip")
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  ;; Move between two dired buffer quickly
  (dired-dwim-target t)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  ;; symlink
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches (if (executable-find "gls")
                              "-Bhl --group-directories-first --almost-all --time-style=iso"
                            "-alh"))
  (dired-use-ls-dired (not (eq system-type 'darwin)))
  (dired-vc-rename-file t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  :config
  (when-let* ((gls (executable-find "gls")))
    (setq insert-directory-program gls
          dired-use-ls-dired t))
  (setq delete-by-moving-to-trash t)
  (with-eval-after-load 'dired-aux
    (add-to-list 'dired-compress-file-alist
                 '("\\.zip\\'" . "zip -j %o %i")))
  (defun thy/dired-copy-files-to-clipboard ()
    "Copy marked Dired files to the system clipboard as file objects."
    (interactive)
    (let* ((files (mapcar #'expand-file-name (dired-get-marked-files)))
           (uri-list (mapconcat (lambda (file) (concat "file://" file)) files "\n")))
      (kill-new (mapconcat #'identity files "\n"))
      (cond
       ((and (eq system-type 'darwin) (executable-find "osascript"))
        ;; Finder paste needs NSPasteboard file URLs, not plain file:// text.
        (let ((script "use framework \"Foundation\"
use framework \"AppKit\"
use scripting additions

on run argv
  set pasteboard to (current application's NSPasteboard's generalPasteboard())
  pasteboard's clearContents()

  set urlArray to (current application's NSMutableArray's array())
  repeat with path in argv
    set nsPath to (current application's NSString's stringWithString_(path))
    set nsURL to (current application's |NSURL|'s fileURLWithPath_(nsPath))
    (urlArray's addObject_(nsURL))
  end repeat
  pasteboard's writeObjects_(urlArray)

  set previousDelimiters to AppleScript's text item delimiters
  set AppleScript's text item delimiters to linefeed
  set joinedPaths to (argv as text)
  set AppleScript's text item delimiters to previousDelimiters

  set joinedString to (current application's NSString's stringWithString_(joinedPaths))
  pasteboard's setString_forType_(joinedString, current application's NSPasteboardTypeString)
end run
"))
          (with-temp-buffer
            (insert script)
            (unless (zerop (apply #'call-process-region
                                  (point-min) (point-max)
                                  "osascript" nil t nil "-" files))
              (user-error "Failed to copy files to clipboard: %s"
                          (replace-regexp-in-string "[[:space:]\n]+\\='" "" (buffer-string)))))))
       ((executable-find "xclip")
        (with-temp-buffer
          (insert uri-list)
          (call-process-region (point-min) (point-max) "xclip" nil nil nil
                               "-i" "-selection" "clipboard" "-t" "text/uri-list"))))
      (message "Copied %d file%s" (length files) (if (= (length files) 1) "" "s"))))
  )

;; Use E to open files with an external command.
(use-package dirvish
  :vc (:url "https://github.com/alexluigit/dirvish"
       :branch "main"
       :rev :newest)
  :demand t
  :preface
  (defconst thy/dirvish-dired-bindings
    '(("o" . dired-do-open)
      ("a" . thy/dired-create-file-or-directory)
      ("z" . dired-do-compress)
      ("Z" . dired-do-compress-to)
      ("y" . dired-do-copy)
      ("p" . dirvish-yank)
      ("P" . dirvish-yank-menu)
      ("Y" . thy/dired-copy-files-to-clipboard)
      ("W" . thy/dired-copy-files-to-clipboard))
    "Key bindings shared by Dirvish and Evil Dired maps.")

  (defconst thy/dirvish-mode-bindings
    (append '(("q" . dirvish-quit)
              ("?" . dirvish-dispatch)
              ("r" . dired-do-rename)
              ("M-f" . dirvish-history-go-forward)
              ("M-b" . dirvish-history-go-backward)
              ("f" . dirvish-fd)
              ("F" . dirvish-fd-switches-menu))
            thy/dirvish-dired-bindings
            '(("N" . dirvish-narrow)
              ("<" . dired-up-directory)
              (">" . dired-find-file)
              ("s" . consult-line)
              ("S" . dirvish-quicksort)
              ("M" . dirvish-mark-menu)
              ("v" . dirvish-vc-menu)
              ("TAB" . dirvish-subtree-toggle)
              ("M-t" . dirvish-layout-toggle)
              ("M-s" . dirvish-setup-menu)
              ("M-e" . dirvish-emerge-mode)))
    "Key bindings shared by regular and Evil Dirvish maps.")

  (defun thy/git-directory-contents-ignored-p (directory)
    "Return non-nil when DIRECTORY contains only Git-ignored files."
    (when (and (file-directory-p directory)
               (not (file-remote-p directory)))
      (with-temp-buffer
        (let ((default-directory (file-name-as-directory directory)))
          (and (eq 0 (process-file
                      "git" nil t nil "ls-files" "--cached" "--others"
                      "--exclude-standard" "--" "."))
               (= (buffer-size) 0)
               (progn
                 (erase-buffer)
                 (and (eq 0 (process-file
                             "git" nil t nil "ls-files" "--others" "--ignored"
                             "--exclude-standard" "--" "."))
                      (> (buffer-size) 0))))))))

  (defun thy/dirvish-yank-with-tramp-rpc (function command details &optional batch)
    "Call FUNCTION after preparing remote Dirvish COMMAND for TRAMP RPC.
DETAILS and BATCH are the remaining arguments to `dirvish-yank--execute'."
    (pcase-let ((`(,_ ,sources ,destination ,_) details))
      (when-let* (((and batch
                        (cl-some #'file-remote-p
                                 (cons destination sources))))
                  (rpc-library (locate-library "tramp-rpc"))
                  (msgpack-library (locate-library "msgpack")))
        (setq command
              (prin1-to-string
               `(progn
                  (add-to-list 'load-path ,(file-name-directory rpc-library))
                  (add-to-list 'load-path ,(file-name-directory msgpack-library))
                  (require 'tramp-rpc)
                  (setq tramp-rpc-deploy-local-cache-directory
                        ,tramp-rpc-deploy-local-cache-directory)
                  (setq tramp-rpc-deploy-git-build-policy
                        ',tramp-rpc-deploy-git-build-policy)
                  ,(read command))))))
    (funcall function command details batch))

  (autoload 'dirvish-emerge-mode "dirvish-emerge" nil t)
  (autoload 'dirvish-history-go-backward "dirvish-history" nil t)
  (autoload 'dirvish-history-go-forward "dirvish-history" nil t)
  (autoload 'dirvish-narrow "dirvish-narrow" nil t)
  (autoload 'dirvish-quick-access "dirvish-quick-access" nil t)
  (autoload 'dirvish-quicksort "dirvish-ls" nil t)
  (autoload 'dirvish-subtree-toggle "dirvish-subtree" nil t)
  (autoload 'dirvish-vc-menu "dirvish-vc" nil t)
  (autoload 'dirvish-yank "dirvish-yank" nil t)
  (autoload 'dirvish-yank-menu "dirvish-yank" nil t)
  :init
  (when-let* ((dirvish-file (locate-library "dirvish"))
              (dirvish-dir (file-name-directory dirvish-file)))
    (add-to-list 'load-path (expand-file-name "extensions" dirvish-dir)))
  :custom-face
  (dirvish-hl-line ((t (:inherit hl-line))))
  (dirvish-collapse-file-face ((t (:height 0.8))))
  (dirvish-collapse-dir-face ((t (:inherit dired-directory :height 0.8))))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/desktop/" "Desktop")
     ("p" "~/project/" "Project")
     ("b" "~/blog/" "Blog")
     ("r" "~/roam/" "Roam")
     ("l" "~/learn/" "Learn")
     ))
  ;; Dirvish mode line.
  (dirvish-mode-line-format
   '(:left (sort omit symlink) :right (thy/file-owner " " file-time index)))
  ;; Dirvish file attributes; collapse folds directory contents.
  (dirvish-attributes
   '(thy/vc-ignored nerd-icons collapse git-msg file-size))
  ;; Dirvish header line.
  (dirvish-header-line-format
   '(:left (path) :right (omit yank vc-info free-space)))
  (dirvish-path-separators '("~" "/" "/"))
  (dirvish-input-throttle 0.05)
  (dirvish-window-fringe 4)
  (dirvish-hide-cursor t) ; 在 wired 下不方便
  ;; Use media preview dispatchers, including the custom dispatchers below.
  (dirvish-preview-dispatchers
   '(office video graffle eps image gif audio epub archive font pdf))
  ;; M-e
  (dirvish-emerge-groups
   '(
     ;; ("Recent files"  (predicate . recent-files-2h))
     ("Documents"     (extensions "pdf" "tex" "bib" "epub"))
     ("Video"         (extensions "mp4" "mkv" "webm"))
     ("Pictures"      (extensions "jpg" "png" "svg" "gif"))
     ("Audio"         (extensions "mp3" "flac" "wav" "ape" "aac"))
     ("Archives"      (extensions "gz" "rar" "zip"))
     ("Office"        (extensions "doc" "docx" "xls" "xlsx" "ppt" "pptx"))))
  (dirvish-default-layout '(1 0.15 0.35))
  ;; (dirvish-preview-disabled-exts '("bin" "exe" "gpg" "elc" "eln" "pdf"))
  :hook
  (dirvish-mode . dired-omit-mode)
  ;; (dirvish-setup . dirvish-emerge-mode)
  :config
  (require 'dirvish-widgets)
  (require 'dirvish-vc)
  (dirvish-define-mode-line thy/file-owner
    "Group and user of the file at point."
    (when-let* ((group (dirvish--format-file-attr 'group-id))
                (user (dirvish--format-file-attr 'user-id)))
      (pcase-let ((`(,gid . ,face) group)
                  (`(,uid . ,_) user))
        (unless (dirvish-prop :remote)
          (when (integerp gid)
            (setq gid (or (group-name gid) gid)))
          (when (integerp uid)
            (setq uid (or (user-login-name uid) uid))))
        (propertize (format "%s:%s" gid uid) 'face face))))
  (dirvish-define-attribute thy/vc-ignored
    "Dim file names ignored by version control."
    :when (and (symbolp (dirvish-prop :vc-backend))
               (not (dirvish-prop :remote)))
    (when (and
           (eq (dirvish-attribute-cache f-name :vc-state) 'ignored)
           (or (not (and (eq (dirvish-prop :vc-backend) 'Git)
                          (eq (car f-type) 'dir)))
                (eq (dirvish-attribute-cache
                        f-name :thy/git-ignore-state
                      (if (or (eq 0 (ignore-errors
                                      (process-file
                                       "git" nil nil nil "-C"
                                       (file-name-directory f-name)
                                       "check-ignore" "-q" "--" f-name)))
                              (thy/git-directory-contents-ignored-p f-name))
                          'ignored
                        'visible))
                    'ignored)))
      (let ((ov (make-overlay f-beg f-end)))
        (overlay-put ov 'face 'dired-ignored)
        `(ov . ,ov))))
  (add-to-list 'dirvish-image-exts "graffle")
  (add-to-list 'dirvish-image-exts "eps")
  (add-to-list 'dirvish-binary-exts "graffle")
  (add-to-list 'dirvish-binary-exts "eps")
  (dirvish-define-preview office (file ext preview-window)
    "Preview Office documents without blocking Dirvish."
    (cond
     ((member ext '("doc" "docx"))
      (if-let* ((textutil (executable-find "textutil")))
          `(shell . (,textutil "-convert" "txt" "-stdout"
                               "-encoding" "UTF-8" "--" ,file))
        '(info . "The `textutil' executable is required for Word previews.")))
     ((member ext '("ppt" "pptx"))
      '(info . "PowerPoint preview is disabled."))
     ((member ext '("xls" "xlsx"))
      (if-let* ((quicklook (executable-find "qlmanage")))
          (let* ((width (dirvish-media--img-size preview-window))
                 (height (dirvish-media--img-size preview-window 'height))
                 (cache-dir (dirvish--img-thumb-name
                             file width ".quicklook"))
                 (cache (expand-file-name
                         (concat (file-name-nondirectory file) ".png")
                         cache-dir)))
            (make-directory cache-dir t)
            (if (and (file-exists-p cache)
                     (not (file-newer-than-file-p file cache)))
                `(img . ,(create-image cache nil nil
                                       :max-width width :max-height height))
              `(cache . (,quicklook "-t" "-s" ,(number-to-string width)
                                    "-o" ,cache-dir ,file))))
        '(info . "Quick Look is required for spreadsheet previews.")))))
  (dirvish-define-preview graffle (file ext preview-window)
    "Preview the JPEG embedded in an OmniGraffle document."
    (when (equal ext "graffle")
      (let* ((width (dirvish-media--img-size preview-window))
             (height (dirvish-media--img-size preview-window 'height))
             (cache-dir (dirvish--img-thumb-name file width ".graffle"))
             (cache (expand-file-name "preview.jpeg" cache-dir)))
        (cond
         ((and (file-exists-p cache)
               (not (file-newer-than-file-p file cache)))
          `(img . ,(create-image cache nil nil
                                :max-width width :max-height height)))
         ((not (executable-find "unzip"))
          '(info . "The `unzip' executable is required to preview Graffle files."))
         ((zerop (call-process "unzip" nil nil nil "-tqq" file "preview.jpeg"))
          `(cache . ("unzip" "-qq" "-DD" "-o" ,file "preview.jpeg"
                     "-d" ,cache-dir)))
         (t '(info . "This Graffle file has no embedded preview.jpeg."))))))
  (dirvish-define-preview eps (file ext preview-window)
    "Preview EPS files using ImageMagick and Ghostscript."
    :require (dirvish-magick-program)
    (when (equal ext "eps")
      (let* ((width (dirvish-media--img-size preview-window))
             (height (dirvish-media--img-size preview-window 'height))
             (cache (dirvish--img-thumb-name file width ".png")))
        (if (and (file-exists-p cache)
                 (not (file-newer-than-file-p file cache)))
            `(img . ,(create-image cache nil nil
                                  :max-width width :max-height height))
          `(cache . (,dirvish-magick-program
                     "-density" "144" ,file
                     "-thumbnail" ,(format "%sx%s" width height)
                     "-background" "white" "-alpha" "remove"
                     ,cache))))))
  (dirvish-override-dired-mode)
  (dolist (binding thy/dirvish-mode-bindings)
    (keymap-set dirvish-mode-map (car binding) (cdr binding)))
  (with-eval-after-load 'dirvish-yank
    (advice-add #'dirvish-yank--execute
                :around #'thy/dirvish-yank-with-tramp-rpc))
  )

;; [dired-x] Extra Dired functionality
;; Primarily configure Dired's omit feature.
(use-package dired-x
  :ensure nil
  :bind (:map dired-mode-map
              ("." . dired-omit-mode))
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; (use-package dired-x
;;   :straight nil
;;   :after dired
;;   :bind (:map dired-mode-map
;;               ("." . dired-omit-mode))
;;   :config
;;   (let ((cmd (cond ((and (eq system-type 'darwin) (display-graphic-p)) "open")
;;                    ((and (eq system-type 'gnu/linux) (display-graphic-p)) "xdg-open")
;;                    ((and (eq system-type 'windows-nt) (display-graphic-p)) "start")
;;                    (t ""))))
;;     (setq dired-guess-shell-alist-user
;;           `(("\\.pdf\\'" ,cmd)
;;             ("\\.docx\\'" ,cmd)
;;             ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
;;             ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
;;             ("\\.\\(?:xcf\\)\\'" ,cmd)
;;             ("\\.csv\\'" ,cmd)
;;             ("\\.tex\\'" ,cmd)
;;             ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
;;             ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd))))
;;
;;   (setq dired-omit-verbose nil
;;         ;; hide dot files
;;         ;;dired-omit-files "^\\..*\\'"
;;         )
;;
;;   ;; Disable the prompt about killing the Dired buffer for a deleted directory.
;;   (setq dired-clean-confirm-killing-deleted-buffers nil)
;;   )

;; dired more colorful
(use-package diredfl
  :ensure t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))
