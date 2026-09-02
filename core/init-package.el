;;; -*- lexical-binding: t -*-

(require 'package)
(require 'subr-x)

;; This must be set before loading use-package.
(setq use-package-enable-imenu-support t)
(require 'use-package)

(defun thy/package-load-env-file (file)
  "Load KEY=VALUE entries from FILE into the process environment."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (unless (or (string-empty-p line)
                      (string-prefix-p "#" line))
            (when (string-match
                   "\\`\\(?:export[[:space:]]+\\)?\\([[:alpha:]_][[:alnum:]_]*\\)[[:space:]]*=[[:space:]]*\\(.*\\)\\'"
                   line)
              (let ((name (match-string 1 line))
                    (value (string-trim (match-string 2 line))))
                (when (and (> (length value) 1)
                           (memq (aref value 0) '(?\" ?\'))
                           (eq (aref value 0) (aref value (1- (length value)))))
                  (setq value (substring value 1 -1)))
                (setenv name value)))))
        (forward-line 1)))))

(thy/package-load-env-file (expand-file-name ".env" user-emacs-directory))

(when-let* ((path (getenv "PATH")))
  (setq exec-path
        (delete-dups (append (parse-colon-path path) (list exec-directory)))))

(require 'url-methods)
(dolist (scheme '("http" "https"))
  (url-scheme-register-proxy scheme))

;; Ensure Homebrew GnuPG is available while bootstrapping package signatures.
(let ((homebrew-bin "/opt/homebrew/bin"))
  (when (and (eq system-type 'darwin)
             (file-executable-p (expand-file-name "gpg" homebrew-bin)))
    (add-to-list 'exec-path homebrew-bin)
    (setenv "PATH" (concat homebrew-bin path-separator (getenv "PATH")))))

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 20)
        ("nongnu" . 10)
        ("melpa" . 0)))

(setq package-native-compile nil)
(setq package-vc-allow-build-commands t)

(defun thy/package-import-default-keyring ()
  "Import Emacs's package keyring when GnuPG is available and uninitialized."
  (when (and package-gnupghome-dir
             (package-check-signature)
             (not (file-directory-p package-gnupghome-dir)))
    (let ((keyring (expand-file-name "package-keyring.gpg" data-directory)))
      (when (file-exists-p keyring)
        (condition-case nil
            (package-import-keyring keyring)
          (epg-error nil))))))

(thy/package-import-default-keyring)

(package-initialize)

;; A deleted package directory also removes cached archive metadata.
(unless package-archive-contents
  (package-refresh-contents))

;; Reconstruct top-level packages when Customize state is intentionally not loaded.
(unless package-selected-packages
  (setq package-selected-packages (package--find-non-dependencies)))

;; Shallow clone package-vc repositories when a branch or tag is specified.
(defun thy/vc-git-clone (fn remote directory rev)
  "Call FN to clone REMOTE into DIRECTORY, shallowly when REV permits it."
  (if (or (not (string-match-p "elpa" directory))
          (null rev))
      (funcall fn remote directory rev)
    (cond
     ((ignore-errors
        ;; First try if rev is a branch/tag name
        ;; https://stackoverflow.com/a/48748567/2163429
        (vc-git--out-ok "clone" "--depth" "1" "--single-branch" "--branch" rev remote directory)))
     ((vc-git--out-ok "clone" "--single-branch" remote directory)
      (let ((default-directory directory))
        (vc-git--out-ok "checkout" rev))))
    directory))

(advice-add 'vc-git-clone :around 'thy/vc-git-clone)

;; use-package settings.
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

;; Work around Emacs bug#77928, which makes `:custom-face' weaker than themes.
(defun thy/apply-face-override (definition)
  "Apply face override DEFINITION and mark its face as customized."
  (apply #'face-spec-set definition)
  (put (car definition) 'face-modified t))

(defun thy/use-package-handler-custom-face-override (name _keyword args rest state)
  "Expand ARGS as face overrides, then process REST for package NAME and STATE."
  (use-package-concat
   (mapcar (lambda (definition)
             `(progn
                (thy/apply-face-override (backquote ,definition))
                ;; A deferred package may define the face after this declaration.
                (with-eval-after-load ',name
                  (thy/apply-face-override (backquote ,definition)))))
           args)
   (use-package-process-keywords name rest state)))

(let* ((probe
        '(use-package thy/custom-face-probe
           :no-require t
           :custom-face
           (thy/custom-face-probe ((t (:weight bold))))))
       (broken-handler
        (string-match-p
         "\\_<face-defface-spec\\_>"
         (prin1-to-string (macroexpand-1 probe)))))
  (if broken-handler
      (unless (advice-member-p #'thy/use-package-handler-custom-face-override
                               #'use-package-handler/:custom-face)
        (advice-add #'use-package-handler/:custom-face :override
                    #'thy/use-package-handler-custom-face-override))
    (when (advice-member-p #'thy/use-package-handler-custom-face-override
                           #'use-package-handler/:custom-face)
      (advice-remove #'use-package-handler/:custom-face
                     #'thy/use-package-handler-custom-face-override))))
