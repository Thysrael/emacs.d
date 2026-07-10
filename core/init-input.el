;; -*- lexical-binding: t; -*-

(use-package macim
  :if (eq system-type 'darwin)
  :vc (macim :url "https://github.com/roife/macim.el"
             :rev "master")
  :commands (macim-mode macim-select-ascii macim-select-other)
  :hook (after-init . macim-mode)
  :preface
  (defvar thy/macim-chinese-punc-chars nil
    "Chinese punctuation characters used by `macim' inline cleanup.")

  (defvar-local thy/macim-inline-english-last-space-pos nil
    "Last inserted space position in `macim' inline mode.")

  (defun thy/macim-remove-head-space-after-chinese-punc (_)
    "Remove leading inline space after Chinese punctuation."
    (when (or (bolp)
              (memq (char-before) thy/macim-chinese-punc-chars))
      (delete-char 1)))

  (defun thy/macim-remove-tail-space-before-chinese-punc (tighten-back-to)
    "Remove trailing inline space before Chinese punctuation back to TIGHTEN-BACK-TO."
    (when (> (point) tighten-back-to)
      (backward-delete-char (1- (- (point) tighten-back-to))))
    (when (and (eq (char-before) ?\s)
               (memq (char-after) thy/macim-chinese-punc-chars))
      (backward-delete-char 1)))

  (defun thy/macim-record-inline-english-space ()
    "Remember the last space inserted by `macim' inline mode."
    (when (eq (char-before) ?\s)
      (setq thy/macim-inline-english-last-space-pos (point))))

  (defun thy/macim-remove-redundant-inline-space ()
    "Remove the extra inline space before Chinese punctuation."
    (when (and (eq thy/macim-inline-english-last-space-pos (1- (point)))
               (memq (char-before) thy/macim-chinese-punc-chars)
               (eq (char-before (1- (point))) ?\s))
      (save-excursion
        (backward-char 2)
        (delete-char 1)
        (setq thy/macim-inline-english-last-space-pos nil)))
    (remove-hook 'post-self-insert-hook #'thy/macim-remove-redundant-inline-space t))

  (defun thy/macim-add-inline-space-cleanup ()
    "Clean one redundant inline space after `macim' switches back to other input."
    (add-hook 'post-self-insert-hook #'thy/macim-remove-redundant-inline-space nil t))

  :config
  (setq thy/macim-chinese-punc-chars
        (mapcar #'string-to-char macim--chinese-punc-list))
  (setq macim-inline-head-handler #'thy/macim-remove-head-space-after-chinese-punc)
  (setq macim-inline-tail-handler #'thy/macim-remove-tail-space-before-chinese-punc)
  (add-hook 'macim-inline-deactivated-hook #'thy/macim-record-inline-english-space)
  (add-hook 'macim-inline-deactivated-hook #'thy/macim-add-inline-space-cleanup)
  (with-eval-after-load 'meow
    (add-hook 'meow-insert-exit-hook #'macim-select-ascii)
    (add-hook 'meow-insert-enter-hook #'macim-context-switch))
  (setq macim-ascii "com.apple.keylayout.ABC"
        macim-other "im.rime.inputmethod.Squirrel.Hans"))
