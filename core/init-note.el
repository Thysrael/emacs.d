;; 提供 wiki 式自下而上的笔记系统
;; rename 似乎没有好的方式
;; delete 同样没有好的方式
;; 所以建议 rename 使用 alias ，delete 使用 deprecated
(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
                   :files (:defaults "extensions/*"))
  :custom
  (org-roam-v2-ack t)

  :bind
  ("C-c n" . hydra-roam/body)
  ("C-c N" . org-roam-node-find)
  ("C-c O" . org-roam-capture)
  ("C-c I" . org-roam--node-insert)
  :config
  (setq org-roam-directory "~/blog/source/roam/")
  (setq org-roam-db-location "~/blog/source/roam/org-roam.db")

  (defhydra hydra-roam (:hint nil :color blue :foreign-keys run)
  "
        Operate^^        Links^^                 Misc^^
  --------------------------------------------------------------
        [_f_] Find        [_b_] Backlinks        [_t_] Tag
        [_i_] Insert      [_B_] BackRecursive    [_a_] Alias
        [_S_] Search      [_s_] ForwardLinks     [_d_] Sync
        ^ ^               ^ ^                    [_o_] Capture
        [_q_] Quit        ^ ^
  "
    ("f" org-roam-node-find)
    ("i" org-roam-node-insert)
    ;; ("b" org-roam-buffer-toggle "Backlinks") ; 展示 backlinks
    ("o" org-roam-capture) ; 在已有 node 内增加内容
    ("S" consult-org-roam-search)
    ("u" org-roam-ui-open)

    ("b" consult-org-roam-backlinks)
    ("B" consult-org-roam-backlinks-recursive)
    ("s" consult-org-roam-forward-links)
    ("a" org-roam-alias-add)
    ;; ("ar" org-roam-alias-remove "Alias Remove")
    ("t" org-roam-tag-add)
    ;; ("tr" org-roam-tag-remove "Tag Remove")
    ("d" org-roam-db-sync)
    ("q" nil)
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
  )

;; org-roam 可视化
(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  :custom
  (org-roam-ui-browser-function 'eaf-open-browser)
  )

(use-package consult-org-roam
  :straight t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
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
