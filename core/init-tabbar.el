;; -*- lexical-binding: t; -*-

;; Show persp list when only one persp exists
(defvar +tab-bar-shows-single-empty-persp t)

;; [tab-bar] Tab bar
(use-package tab-bar
  :bind
  ("<f10>"  . tab-next)
  ("s-<right>" . tab-next)
  ("s-<left>" . tab-previous)
  ("s-l" . tab-next)
  ("s-h" . tab-previous)
  ;; Turn on tab-bar-mode in early-init to speed-up
  :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 30
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t)

  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; truncate for [tab name] and add count
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                (if (> count 1)
                    (concat truncated-tab-name "(" (number-to-string count) ")")
                  truncated-tab-name))))

  ;; Add spaces for tab-name
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  ;; cache for persp indicator
  (with-eval-after-load 'persp-mode
    (defvar +tab-bar-persp-indicator-cache nil)
    (add-hook! (persp-activated-functions persp-names-cache-changed-functions)
      (defun +tab-bar-update-persp-indicator (&rest _)
        (when-let* ((persp-list (persp-names-current-frame-fast-ordered))
                    (len (length persp-list)))
          (setq +tab-bar-persp-indicator-cache
                (when (or (> len 1) +tab-bar-shows-single-empty-persp)
                  (let* ((cur-persp-name (safe-persp-name (get-current-persp)))
                         (cur-pos (cl-position cur-persp-name persp-list))
                         (before (seq-subseq persp-list 0 cur-pos))
                         (before-joined (concat (string-join before " ") " "))
                         (after (seq-subseq persp-list (1+ cur-pos)))
                         (after-joined (concat " " (string-join after " ")))
                         (face '(:inherit font-lock-constant-face :inverse-video t))
                         (text (concat (propertize (concat " " (when before before-joined) "") 'face face)
                                       (propertize (concat cur-persp-name)
                                                   'face (append face '(:weight ultra-bold :underline t :background "white")))
                                       (propertize (concat (when after after-joined) " ") 'face face))))
                    `((tab-bar-persp menu-item
                                     ,text
                                     ignore
                                     :help ,(concat "Current persp: " cur-persp-name))))))
          (force-mode-line-update t)
          +tab-bar-persp-indicator-cache))))

  (defun +tab-bar-persp-indicator ()
    (when (bound-and-true-p persp-mode)
      (or +tab-bar-persp-indicator-cache
          (+tab-bar-update-persp-indicator))))

  ;; cache for telega indicator
;;   (with-eval-after-load 'telega
;;     (defvar +tab-bar-telega-indicator-cache nil)
;;
;;     (add-hook! (telega-connection-state-hook telega-kill-hook telega-online-status-hook)
;;       (defun +tab-bar-telega-icon-update (&rest rest)
;;         (when (buffer-live-p telega-server--buffer)
;;           (let* ((me-user (telega-user-me 'locally))
;;                  (online-p (and me-user (telega-user-online-p me-user)))
;;                  ;; reactions
;;                  (reactions-chats (telega-filter-chats telega--ordered-chats '(and is-known unread-reactions)))
;;                  (reactions-count (apply '+ (mapcar (telega--tl-prop :unread_reaction_count) reactions-chats)))
;;                  ;; mentioned
;;                  (mentioned-chats (telega-filter-chats telega--ordered-chats '(mention)))
;;                  (mentioned-count (apply '+ (mapcar (telega--tl-prop :unread_mention_count) mentioned-chats)))
;;                  ;; unread
;;                  (unmuted-count (or (plist-get telega--unread-chat-count :unread_unmuted_count) 0))
;;                  (mentioned-unmuted-chats (telega-filter-chats telega--ordered-chats '(and mention unmuted)))
;;                  (true-unmuted-count (- unmuted-count (length mentioned-unmuted-chats)))
;;                  (text (propertize (concat " " telega-symbol-telegram " "
;;                                            (when (> true-unmuted-count 0)
;;                                              (concat "●" (number-to-string true-unmuted-count) " "))
;;                                            (when (> mentioned-count 0)
;;                                              (concat "@" (number-to-string mentioned-count) " "))
;;                                            (when (> reactions-count 0)
;;                                              (concat "❤" (number-to-string reactions-count) " ")))
;;                                    'face `(:inherit font-lock-keyword-face :inverse-video ,online-p)))
;;                  (first-name (plist-get me-user :first_name))
;;                  (last-name (plist-get me-user :last_name))
;;                  (help-echo (concat "Current User: " first-name " " last-name "\n"
;;                                     "Status: " (if online-p "online" "offline"))))
;;             (setq +tab-bar-telega-indicator-cache
;;                   `((tab-bar-persp menu-item
;;                                    ,text
;;                                    ignore
;;                                    :help ,help-echo))))
;;           (force-mode-line-update t)
;;           +tab-bar-telega-indicator-cache)
;;         ))
;;
;;   (advice-add 'telega--on-updateUserStatus :after #'+tab-bar-telega-icon-update)
;;   (advice-add 'telega--on-updateUnreadChatCount :after #'+tab-bar-telega-icon-update)
;;   (advice-add 'telega--on-updateChatUnreadMentionCount :after #'+tab-bar-telega-icon-update)
;;   (advice-add 'telega--on-updateChatUnreadReactionCount :after #'+tab-bar-telega-icon-update))
;;
;; (defun +tab-bar-telega-icon ()
;;   (when (and (fboundp 'telega-server-live-p)
;;              (telega-server-live-p))
;;     (or +tab-bar-telega-indicator-cache
;;         (+tab-bar-telega-icon-update))))
;;
;; (defun +tab-bar-copilot-icon ()
;;   (when (bound-and-true-p copilot-mode)
;;     (propertize " α " 'face '(:inherit font-lock-doc-face :inverse-video t))))

;; cache for org-pomodoro
;; (with-eval-after-load 'org-pomodoro
;;   (defvar +tab-bar-org-pomodoro-indicator-cache nil)
;;
;;   (add-hook! (org-pomodoro-started-hook org-pomodoro-finished-hook org-pomodoro-overtime-hook
;;                                         org-pomodoro-killed-hook org-pomodoro-break-finished-hook
;;                                         org-pomodoro-long-break-finished-hook org-pomodoro-killed-hook)
;;     (defun +tab-bar-org-pomodoro-indicator-update (&rest _)
;;       (setq +tab-bar-org-pomodoro-indicator-cache
;;             (cl-case org-pomodoro-state
;;               (:none
;;                (propertize " Stop " 'face '(:inherit font-lock-comment-face :inverse-video t)))
;;               (:pomodoro
;;                (propertize " Pomo " 'face '(:inherit org-pomodoro-mode-line :inverse-video t)))
;;               (:overtime
;;                (propertize " Over " 'face '(:inherit org-pomodoro-mode-line-overtime :inverse-video t)))
;;               (:short-break
;;                (propertize " Short break " 'face '(:inherit org-pomodoro-mode-line-break :inverse-video t)))
;;               (:long-break
;;                (propertize " Long break " 'face '(:inherit org-pomodoro-mode-line-break :inverse-video t))))))
;;     ))
;;
;; (defun +tab-bar-org-pomodoro-indicator ()
;;   (when (fboundp 'org-pomodoro-active-p)
;;     (or +tab-bar-org-pomodoro-indicator-cache
;;         (+tab-bar-org-pomodoro-indicator-update))))

;; ime
;; (defvar +tab-bar-rime-active-hint (propertize " ⭘ " 'face '(:inherit rime-indicator-face :inverse-video t)))
;; (defvar +tab-bar-rime-inactive-hint (propertize " ● " 'face '(:inherit rime-indicator-face :inverse-video t)))
;; (defvar +tab-bar-no-ime-hint (propertize " ⭘ " 'face '(:inherit rime-indicator-dim-face :inverse-video t)))
;; (defun +tab-bar-rime-indicator ()
;;   (let ((text-help (if (and (equal current-input-method "rime")
;;                             (bound-and-true-p rime-mode))
;;                        (if (and (rime--should-enable-p)
;;                                 (not (rime--should-inline-ascii-p)))
;;                            (cons +tab-bar-rime-active-hint "Rime Enabled")
;;                          (cons +tab-bar-rime-inactive-hint "Rime Disabled"))
;;                      (cons +tab-bar-no-ime-hint "No IME"))))
;;     `((tab-bar-rime menu-item
;;                     ,(car text-help)
;;                     ignore
;;                     :help ,(cdr text-help)))))


(defun +hide-tab-bar ()
  (interactive)
  (setq tab-bar-format nil))

(defun-call! +show-tab-bbar ()
  (interactive)
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator
                                             tab-bar-format-align-right
                                             ;; +tab-bar-org-pomodoro-indicator
                                             ;; +tab-bar-copilot-icon
                                             ;; +tab-bar-telega-icon
                                             +tab-bar-persp-indicator
                                             ;; meow-indicator
                                             ;; +tab-bar-rime-indicator
                                             ))
  (tab-bar--update-tab-bar-lines))

;; WORKAROUND: fresh tab-bar for daemon
(when (daemonp)
  (add-hook 'after-make-frame-functions
            #'(lambda (&rest _) (force-mode-line-update))))
)

(defhydra window (
                 :hint nil ; 只显示注释字符串，不显示绑定信息
                 :color blue ; 执行完一次后就退出
                 :foreign-keys run ; 如果不在 hydra 按键内，则执行，并不退出 hydra
                 )
  "
     Tab^^          Winner^^        Resize^^          Desktop^^
  -----------------------------------------------------------------------
     [_c_] Switch   [_u_] Undo      [_=_] Enlarge     [_s_] SaveDefault
     [_0_] Close    [_U_] Redo      [_-_] Shrink      [_l_] Load
     [_2_] New      ^ ^             [_+_] EnlargeV    ^ ^
     [_R_] Rename   ^ ^             [___] ShrinkV     [_q_] Quit
  "
  ("c" tab-switch)
  ("0" tab-close :color red)
  ("2" tab-new)
  ("R" tab-rename :color red)

  ("u" winner-undo :color red)
  ("U" winner-redo :color red)

  ("=" enlarge-window :color red)
  ("-" shrink-window :color red)
  ("+" enlarge-window-horizontally :color red)
  ("_" shrink-window-horizontally :color red)

  ("s" desktop-save-in-desktop-dir)
  ("l" desktop-read)

  ("q" nil))

(global-set-key (kbd "C-x t") 'window/body)
