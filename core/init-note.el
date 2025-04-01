;; 提供 wiki 式自下而上的笔记系统
;; rename 似乎没有好的方式
;; delete 同样没有好的方式
;; 所以建议 rename 使用 alias ，delete 使用 deprecated
(use-package org-roam
  :ensure t
  :custom
  (org-roam-v2-ack t)

  :bind
  ("C-c N" . transient-roam)
  ("C-c n" . org-roam-node-find)
  ("C-c K" . org-roam-capture)
  ("C-c I" . org-roam-node-insert)
  :config
  (setq org-roam-directory "~/roam/")
  (setq org-roam-db-location "~/roam/org-roam.db")

  (transient-define-prefix transient-roam ()
    "transient for org roam"
    [
     ["Operate"
      ("f" "Find" org-roam-node-find)
      ("i" "Insert" org-roam-node-insert)
      ("S" "Search" consult-org-roam-search)
      ]
     
     ["Links"
      ("b" "Back" consult-org-roam-backlinks)
      ("B" "BackR"consult-org-roam-backlinks-recursive)
      ("s" "Forward" consult-org-roam-forward-links)
      ]

     ["Misc"
      ("a" "Alias" org-roam-alias-add)
      ("t" "Tag" org-roam-tag-add)
      ("d" "Sync" org-roam-db-sync)
      ("o" "Capture" org-roam-capture)
      ]
     ]
    )
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; 可以用 file-local 来确定要不要显示未链接的部分
  ;; (setq org-roam-mode-sections
  ;;       (list #'org-roam-backlinks-section
  ;;             #'org-roam-unlinked-references-section
  ;;             ))
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  ;; add
  (defun org-roam-backlinks-query* (NODE)
    "Gets the backlinks of NODE with `org-roam-db-query'."
    (org-roam-db-query
     [:select [source dest]
		      :from links
		      :where (= dest $s1)
		      :and (= type "id")]
     (org-roam-node-id NODE)))

  (defun org-roam-backlinks-p (SOURCE NODE)
    "Predicate function that checks if NODE is a backlink of SOURCE."
    (let* ((source-id (org-roam-node-id SOURCE))
	       (backlinks (org-roam-backlinks-query* SOURCE))
	       (id (org-roam-node-id NODE))
	       (id-list (list id source-id)))
      (member id-list backlinks)))

  (defun org-roam-backlinks--read-node-backlinks (source)
    "Runs `org-roam-node-read' on the backlinks of SOURCE.
 The predicate used as `org-roam-node-read''s filter-fn is
 `org-roam-backlinks-p'."
    (org-roam-node-read nil (apply-partially #'org-roam-backlinks-p source)))

  (defun org-roam-backlinks-node-read (entry)
    "Read a NODE and run `org-roam-backlinks--read-node-backlinks'."
    (let* ((node (get-text-property 0 'node entry))
           (backlink (org-roam-backlinks--read-node-backlinks node)))
      (find-file (org-roam-node-file backlink))))

  (with-eval-after-load 'embark
    (defvar-keymap embark-org-roam-map
      :doc "Keymap for Embark org roam actions."
      :parent embark-general-map
      "i" #'org-roam-node-insert
      "s" #'embark-collect
      "b" #'org-roam-backlinks-node-read)
    (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-map)))
  )

;; org-roam 可视化
;; (use-package org-roam-ui
;;   :straight
;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;   :after org-roam
;;   ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;   ;;         a hookable mode anymore, you're advised to pick something yourself
;;   ;;         if you don't care about startup time, use
;;   ;;  :hook (after-init . org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t)
;;   ;; :custom
;;   ;; (org-roam-ui-browser-function 'eaf-open-browser)
;;   )

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :custom
  ;; Activate the minor mode
  (consult-org-roam-mode t)
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  ;; :config
  ;; ;; Eventually suppress previewing for certain functions
  ;; (consult-customize
  ;;  consult-org-roam-forward-links
  ;;  :preview-key "M-.")
  ;; :bind
  ;; Define some convenient keybindings as an addition
  ;; ("C-c n e" . consult-org-roam-file-find)
  ;; ("C-c n b" . consult-org-roam-backlinks)
  ;; ("C-c n B" . consult-org-roam-backlinks-recursive)
  ;; ("C-c n l" . consult-org-roam-forward-links)
  ;; ("C-c n r" . consult-org-roam-search)
  )
