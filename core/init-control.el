;;; -*- lexical-binding: t -*-

(use-package org
  :straight t
  :bind
  ("C-c o" . org-capture) ; org-capture
  :config
  (setq org-default-notes-file "~/learn/org/inbox.org")
  (setq org-capture-templates '())
  (add-to-list 'org-capture-templates
               '("b" "Book Reading" entry
                 (file+olp "" "Reading")
                 "* TODO %^{Book Name} %^g\n%U\n%?"))
  (add-to-list 'org-capture-templates
               '("e" "Emacs Config" entry
                 (file+olp "" "Emacs Config")
                 "* TODO %^{Config or Bug} %^g\n%U\n%?"))
  (add-to-list 'org-capture-templates
               '("p" "Blog Post" entry
                 (file+olp "" "Blog")
                 "* TODO %^{Idea} %^g\n%U\n%?"))
  (add-to-list 'org-capture-templates
               '("m" "Misc" entry
                 (file+olp "" "Misc")
                 "* TODO %^{Misc} %^g\n%U\n%?"))
  (add-to-list 'org-capture-templates
               '("w" "Word" entry
                 (file+olp "~/learn/org/words.org" "Inbox")
                 "* TODO %^{Word}\n%?"))
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "PEND(p@/!)" "|" "DONE(d!)" "ABRT(a@/!)")))
  )
