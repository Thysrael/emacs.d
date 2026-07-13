;;; -*- lexical-binding: t -*-

(require 'package)
(require 'subr-x)

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

(setq package-native-compile t)
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
(setq use-package-enable-imenu-support t)
