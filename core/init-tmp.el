;; -*- lexical-binding: t; -*-

;; 都挺好，就是默认有些丑
;; (use-package window-stool
;;   :vc (:url "https://github.com/JasZhe/window-stool" :rev "main")
;;   :hook prog-mode
;;   :custom
;;   (window-stool-n-from-top 2)
;;   (window-stool-use-overlays nil)
;;   :config
;;   ;; 让 side-window 在 ace-window 表现的更自然
;;   (with-eval-after-load 'ace-window
;;     (push " \*Window Stool\*" aw-ignored-buffers)
;;     )
;;   )

;; [zoom] Managing the window sizes automatically
;; (use-package zoom
;;   :hook (window-setup . zoom-mode)
;;   :config
;;   (timeout-throttle! 'zoom--handler 0.1))

;; (use-package org-tree-slide
;;   :straight t
;;   :bind
;;   (:map org-tree-slide-mode-map
;;         ("<f9>" . org-tree-slide-move-previous-tree)
;;         ("<f10>" . org-tree-slide-move-next-tree)
;;         )
;;   :hook
;;   (org-tree-slide-mode . (lambda () (setq-local org-num-mode -1)))
;;   :config
;;   (setq org-tree-slide-heading-emphasis t
;;         org-tree-slide-content-margin-top 1
;;         ))

;; (use-package nano-vertico
;;   :straight (nano-vertico :type git :host github :repo "rougier/nano-vertico"))

;; 无法实时预览，所以功能不强
;; (use-package dogears
;;   :straight t
;;   :hook (after-init . dogears-mode)
;;   :config
;;   (setq dogears-idle 1
;;         dogears-limit 200
;;         dogears-position-delta 20)
;;   (setq dogears-functions '(find-file recenter-top-bottom
;;                                       other-window switch-to-buffer
;;                                       aw-select toggle-window-split
;;                                       windmove-do-window-select
;;                                       pager-page-down pager-page-up
;;                                       tab-bar-select-tab
;;                                       pop-to-mark-command
;;                                       pop-global-mark
;;                                       goto-last-change
;;                                       xref-go-back
;;                                       xref-find-definitions
;;                                       xref-find-references)))

;; 只能按日展示，且有股塑料味儿
;; (use-package org-analyzer
;;   :straight t
;;   :init
;;   (setq org-analyzer-org-directory "~/learn/org/")
;;   (setq org-analyzer-process-buffer "~/learn/org/agenda.org_archive"))
