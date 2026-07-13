;;; -*- lexical-binding: t -*-

;;; minibuffer
;; Show minibuffer completions vertically with Vertico.
(use-package vertico
  ;; :ensure (vertico :files ("extensions/*.el"))
  :ensure t
  :bind
  (:map vertico-map
        ("TAB" . minibuffer-complete)
        ("<tab>" . minibuffer-complete) ; Both TAB spellings are needed.
        ("C-r" . vertico-repeat-select) ; Select from command history with arguments.
        ("C-<return>" . vertico-exit-input)) ; Exit with the raw input.
  :hook
  ((after-init . vertico-mode))
  :defines
  (crm-separator)
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-resize nil)
  :config
  ;; Make `completing-read-multiple' prompts easier to distinguish.
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""  crm-separator)
                              (car args))
                      (cdr args))))
  (advice-add #'org-olpath-completing-read :around
              (lambda (orig-fun &rest args)
                (minibuffer-with-setup-hook
                    (lambda () (setq-local completion-styles '(basic)))
                  (apply orig-fun args))))
  )

;; Directory-aware navigation and deletion while completing file names.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

;; Save minibuffer commands together with their arguments.
(use-package vertico-repeat
  :ensure nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

;; Add icons to completion candidates.
(use-package nerd-icons-completion
  :ensure t
  :hook
  ((after-init . nerd-icons-completion-mode)
   (marginalia-mode . nerd-icons-completion-marginalia-setup))
  )

;; Flexible minibuffer matching styles.
(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles thy/basic-remote thy/orderless-basic))))
  (completion-ignore-case t)
  (completion-styles '(thy/orderless-basic))
  (orderless-component-separator "[ &]")
  (orderless-style-dispatchers '(thy/orderless-dispatch))
  :config
  ;; Select different completion styles with prefix or suffix markers.
  (defun thy/orderless-dispatch (pattern _index _total)
    "Dispatch PATTERN to an Orderless matching style."
    (cond
     ;; A trailing $ anchors the match to the end.
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore a bare ! so it can prepare the next negative pattern.
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; A leading ! excludes candidates matching the rest of the pattern.
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Match while ignoring diacritics and similar character variants.
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching.
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching, without regexp interpretation.
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching: characters must appear in order, but not contiguously.
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
     ;; Annotation matching can be expensive; use it only for filtering.
     ((string-prefix-p "@" pattern) `(orderless-annotation . ,(substring pattern 1)))
     ((string-suffix-p "@" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
     ))

  ;; Use basic completion for remote files only.
  (defun thy/basic-remote-try-completion (string table pred point)
    "Try basic completion for remote file STRING in TABLE with PRED at POINT."
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun thy/basic-remote-all-completions (string table pred point)
    "Return basic completions for remote file STRING in TABLE with PRED at POINT."
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list
   'completion-styles-alist
   '(thy/basic-remote
     thy/basic-remote-try-completion
     thy/basic-remote-all-completions
     "Use basic completion on remote files only"))

  (defun thy/orderless-basic-all (str table pred point)
    "Return Orderless completions for STR, falling back to basic completion."
    (or (orderless-all-completions str table pred point)
        (completion-basic-all-completions str table pred point)))

  (defun thy/orderless-basic-try (str table pred point)
    "Try basic completion for STR, falling back to Orderless completion."
    (or (completion-basic-try-completion str table pred point)
        (orderless-try-completion str table pred point)))

  (add-to-list 'completion-styles-alist
               '(thy/orderless-basic
                  thy/orderless-basic-try
                  thy/orderless-basic-all
                  "Unholy mix of Orderless and Basic.")))

;; Match Chinese candidates by pinyin initials.
(use-package pinyinlib
  :ensure t
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun thy/completion-regexp-pinyin (str)
    "Build an Orderless regexp matching Chinese pinyin initials from STR."
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'thy/completion-regexp-pinyin))

;; Show richer annotations for minibuffer candidates.
(use-package marginalia
  :ensure t
  :hook (vertico-mode . marginalia-mode))

;; Context actions for minibuffer candidates and objects at point.
;; Embark is especially useful for org-headline candidates.
(use-package embark
  :ensure t
  :bind (("C-;" . embark-act)
         ("C-c ; e" . embark-export)
         ("C-c ; c" . embark-collect)
         :map embark-general-map
         ("/" . consult-line)
         ("g" . consult-ripgrep))
  :defines (wgrep-change-to-wgrep-mode)
  :init
  (setq prefix-help-command 'embark-prefix-help-command)
  )

;; Integrate Embark collections with Consult preview.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  )

;; With `C-u', `consult-ripgrep' can include ignored files.
(use-package consult
  :ensure t
  :bind
  ([remap bookmark-jump]                 . consult-bookmark)
  ([remap list-registers]                . consult-register)
  ([remap goto-line]                     . consult-goto-line)
  ([remap imenu]                         . consult-imenu)
  ("C-c i"                               . consult-imenu)
  ;; ("C-c I"                               . consult-imenu-multi)
  ;; ("C-c o"                               . consult-outline)
  ("C-c b"                               . consult-bookmark)
  ([remap locate]                        . consult-locate)
  ([remap load-theme]                    . consult-theme)
  ([remap man]                           . consult-man)
  ([remap recentf-open-files]            . consult-recent-file)
  ([remap switch-to-buffer]              . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
  ([remap yank-pop]                      . consult-yank-pop)
  ([remap project-switch-to-buffer]      . consult-project-buffer)
  ("C-c s"                               . consult-ripgrep) ; Global search.
  ("C-c p"                               . consult-fd) ; Global file search.
  ("C-s"                                 . consult-line) ; Buffer-local search.
  :custom
  (consult-async-min-input 2)
  (consult-narrow-key "<")
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  ;; Replace multi-occur with consult-multi-occur.
  (advice-add #'multi-occur :override #'consult-multi-occur)

  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (defun thy/consult-ripgrep-single-file ()
    "Call `consult-ripgrep' for the current buffer file."
    (interactive)
    (let ((consult-project-function (lambda (_dir) nil)))
      (consult-ripgrep (list (shell-quote-argument buffer-file-name)))))
  )

;; Insert paths quickly in the minibuffer.
(use-package consult-dir
  :ensure t
  :bind (([remap list-directory] . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
  )

;;; misc
;; Make URLs and email addresses clickable.
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

(use-package picture
  :ensure nil
  :bind
  (:map picture-mode-map
        ("C-f" . right-char)))

(use-package artist
  :ensure nil
  :bind
  (:map artist-mode-map
        ("C-f" . right-char)))
