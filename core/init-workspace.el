;; [persp-mode] Windows/buffers sets shared among frames + save/load.
(use-package persp-mode
  :defines 
  (recentf-exclude)
  :hook
  (after-init . persp-mode)
  :bind
  ("C-x t" . persp/body)
  ("<f9>"  . persp-next)
  ("s-<up>" . persp-prev)
  ("s-<down>" . persp-next)
  :init 
  (setq persp-keymap-prefix nil)
  ;; :hook
  ;; (kill-emacs-hook . persp-state-save) ; 设置自动保存
  ;; (persp-mode . +load-last-persp) ; 设置自动加载
  :config
  (setq
   persp-auto-save-num-of-backups 0 ; 只维护 1 份备份
   persp-autokill-buffer-on-remove 'kill-weak ; 当所有的 persp 都不持有 buffer 的时候，就将其删除
   persp-reset-windows-on-nil-window-conf nil
   persp-set-last-persp-for-new-frames t
   persp-remove-buffers-from-nil-persp-behaviour nil
   persp-nil-name "⊥" ; persp-nil 是默认 persp
   persp-auto-resume-time 0 ; 不会自动恢复到某个 persp
   persp-auto-save-opt 0 ; 不自动保存
   )

  (defun +load-last-persp ()
    "Load last persp."
    (interactive)
    (persp-load-state-from-file))

  (defun +load-persp-by-name ()
    "Load persp by name"
    (interactive)
    (persp-load-from-file-by-names))

  ;; Don't save [dead] [temp] [remote]
  (add-hook! persp-filter-save-buffers-functions
    (defun +persp-ignore-dead-or-temp-buffers (b)
      "Ignore dead or temp buffers."
      (or (not (buffer-live-p b))
          (string-prefix-p "*" (buffer-name b)))))

  (add-hook! persp-filter-save-buffers-functions
    (defun +persp-ignore-more-temp-buffers (b)
      "Ignore more temporary buffers."
      (let ((bname (file-name-nondirectory (buffer-name b))))
        (or (string-prefix-p ".newsrc" bname)
            (string-prefix-p "magit" bname)
            (string-prefix-p "COMMIT_EDITMSG" bname)
            (string-prefix-p "Pfuture-Callback" bname)
            (string-prefix-p "treemacs-persist" bname)
            (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
            (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)))))

  ;; (add-hook! persp-filter-save-buffers-functions
  ;;   (defun +persp-ignore-remote-buffers (buf)
  ;;     "Ignore remote buffers, which cause errors"
  ;;     (let ((dir (buffer-local-value 'default-directory buf)))
  ;;       (ignore-errors (file-remote-p dir)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Compile integration
  (persp-def-buffer-save/load
   :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
   :save-vars '(major-mode default-directory compilation-directory
                           compilation-environment compilation-arguments))

  ;; Eww integration
  (persp-def-buffer-save/load
   :mode 'eww-mode :tag-symbol 'def-eww-status-buffer
   :save-vars '(major-mode eww-history eww-data eww-history-position)
   :after-load-function
   #'(lambda (b &rest _)
       (let ((cur-buf (current-buffer)))
         (with-current-buffer b
           (when-let ((url (plist-get eww-data :url)))
             (eww url nil)))
         ;; restore buffer
         (switch-to-buffer cur-buf))))

  ;; Tab bar integration
  (with-eval-after-load 'tab-bar
    ;; Save the current workspace's tab bar data.
    (add-hook! persp-before-deactivate-functions
      (defun +persp-save-tab-bar-before-switching (_)
        (set-persp-parameter 'tab-bar-tabs (tab-bar-tabs))
        (set-persp-parameter 'tab-bar-closed-tabs tab-bar-closed-tabs)))
    ;; Restores the tab bar data of the workspace we have just switched to.
    (add-hook! persp-activated-functions
      (defun +persp-restore-tab-bar-after-switching (_)
        (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
        (setq tab-bar-closed-tabs (persp-parameter 'tab-bar-closed-tabs))))

    (add-hook! persp-after-load-state-functions
      (defun +persp-load-tab-bar-config-from-file (&rest _)
        (when (and (persp-parameter 'tab-bar-tabs)
                   (not tab-bar-mode))
          (tab-bar-mode 1))))
    )

  ;; Filter frame parameters
  (setq +persp-filter-parameters-on-save
        '((tab-bar-tabs . (lambda (conf) (frameset-filter-tabs conf nil nil t)))
          (winner-ring . ignore)))

  (defadvice! +persp--filter-frame-parameters-on-save-a (fn &rest args)
    :around #'persp-save-state-to-file
    (let ((all-persp-confs (make-hash-table))
          (ret-val))
      (dolist (persp (hash-table-values *persp-hash*))
        (let ((cur-persp-confs (make-hash-table)))
          (cl-loop for (tag . filter) in +persp-filter-parameters-on-save
                   do (let ((old (persp-parameter tag persp)))
                        (puthash persp old cur-persp-confs)
                        (set-persp-parameter tag (funcall filter old) persp)))
          (puthash persp cur-persp-confs all-persp-confs)))
      (setq ret-val (apply fn args))
      (dolist (persp (hash-table-values *persp-hash*))
        (cl-loop for (tag . filter) in +persp-filter-parameters-on-save
                 do (let* ((cur-persp-confs (gethash persp all-persp-confs))
                           (old (gethash tag cur-persp-confs)))
                      (set-persp-parameter tag old persp))))
      ret-val))

  ;; Per-workspace [winner-mode] history
  (with-eval-after-load 'winner
    (add-to-list 'window-persistent-parameters '(winner-ring . t))

    (add-hook! persp-before-deactivate-functions
      (defun +persp-save-winner-before-switching (_)
        (when (get-current-persp)
          (set-persp-parameter
           'winner-ring (list winner-currents
                              winner-ring-alist
                              winner-pending-undo-ring)))))

    (add-hook! persp-activated-functions
      (defun +persp-restore-winner-after-switching (_)
        (cl-destructuring-bind
            (currents alist pending-undo-ring)
            (or (persp-parameter 'winner-ring) (list nil nil nil))
          (setq winner-undo-frame nil
                winner-currents currents
                winner-ring-alist alist
                winner-pending-undo-ring pending-undo-ring))))

    ;; HACK: `pp' is slow, replace it with prin1
    (+advice-pp-to-prin1! 'persp-save-state-to-file)
    )

  ;; Visual selection surviving workspace changes
  (add-hook 'persp-before-deactivate-functions #'deactivate-mark)

  ;; WORKAROUND: ace-window
  (add-hook! persp-activated-functions
    (defun +persp-update-ace-window-config (&rest _)
      (aw-update)))
  )

;; Bookmark Register Rectangle
(defhydra persp (
                 :hint nil ; 只显示注释字符串，不显示绑定信息
                 :color blue ; 执行完一次后就退出
                 :foreign-keys run ; 如果不在 hydra 按键内，则执行，并不退出 hydra
                 )
  "
        Load/Save^^        Manage^^        Move^^         Tab^^          Winner^^
  -------------------------------------------------------------------------------------
        [_A_] Load All    [_l_] List       [_n_] Next     [_s_] Switch   [_u_] Undo
        [_S_] Save        [_k_] Kill       [_p_] Prev     [_0_] Close    [_U_] Redo
        [_L_] Load        [_r_] Rename     [_b_] Buffer   [_2_] New
        [_q_] quit        [_c_] Create     ^ ^            [_R_] Rename 
  "
  ("A" +load-last-persp)
  ("S" persp-save-state-to-file)
  ("L" +load-persp-by-name)

  ("l" persp-window-switch)
  ("k" persp-kill)
  ("r" persp-rename)
  ("c" persp-frame-switch)

  ("n" persp-next :color red)
  ("p" persp-prev :color red)
  ("b" persp-switch-to-buffer)

  ("s" tab-switch)
  ("0" tab-close)
  ("2" tab-new)
  ("R" tab-rename)

  ("u" winner-undo :color red)
  ("U" winner-redo :color red)

  ("q" nil))

