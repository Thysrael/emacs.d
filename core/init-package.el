;;; -*- lexical-binding: t -*-

(require 'package)

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

(package-initialize)

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
