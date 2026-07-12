;; -*- lexical-binding: t; -*-

(defun thy/consult-obsidian-linked-note (files prompt)
  "Select one of FILES with Consult preview using PROMPT."
  (unless (obsidian-file-p)
    (user-error "Current buffer is not an Obsidian note"))
  (if-let* ((files (delete-dups (seq-filter #'file-exists-p files)))
            (default-directory obsidian-directory)
            (candidates (mapcar #'obsidian-file-relative-name files))
            (choice (consult--read candidates
                                   :prompt prompt
                                   :require-match t
                                   :category 'file
                                   :state (consult--file-preview))))
      (find-file (expand-file-name choice obsidian-directory))
    (message "No linked notes found")))

(defun thy/consult-obsidian-backlinks ()
  "Select and preview a note linking to the current note."
  (interactive)
  (thy/consult-obsidian-linked-note
   (hash-table-keys (obsidian-backlinks))
   "Backlinks: "))

(defun thy/consult-obsidian-forward-links ()
  "Select and preview a note linked from the current note."
  (interactive)
  (let* ((metadata (obsidian-file-metadata))
         (links (gethash 'links metadata)))
    (thy/consult-obsidian-linked-note
     (if links (hash-table-keys links) nil)
     "Forward links: ")))

(transient-define-prefix thy/note-transient ()
  "Transient for Obsidian note commands."
  [["Find"
    ("f" "Note" consult-notes)
    ("s" "Search" consult-notes-search-in-all-notes)
    ("t" "Tag" obsidian-find-tag)]
   ["Links"
    ("b" "Backlinks" thy/consult-obsidian-backlinks)
    ("l" "Forward" thy/consult-obsidian-forward-links)
    ("i" "Insert" obsidian-insert-wikilink)
    ("o" "Follow" obsidian-follow-link-at-point)]
   ["Create"
    ("c" "Note" obsidian-capture)
    ("d" "Daily" obsidian-daily-note)]
   ["Vault"
    ("r" "Rescan" obsidian-rescan-cache)
    ("j" "Jump back" obsidian-jump-back)]])

(use-package consult-notes
  :ensure t
  :commands (consult-notes consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   '(("Obsidian" ?o "~/Documents/obsidian/content/")))
  (consult-notes-use-rg t))

(use-package obsidian
  :ensure t
  :commands (obsidian-backlinks obsidian-file-metadata obsidian-file-p)
  :hook ((markdown-mode markdown-ts-mode) . obsidian-enable-minor-mode)
  :init
  (setq elgrep-data-file nil)
  :custom
  (obsidian-directory "~/Documents/obsidian/content/")
  (obsidian-include-hidden-files nil)
  (obsidian-use-update-timer nil))
