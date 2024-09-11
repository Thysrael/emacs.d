;; -*- lexical-binding: t; -*-

;; [tab-bar] Tab bar
(use-package tab-bar
  :bind
  ("<f10>"  . tab-next)
  ("s-<right>" . tab-next)
  ("s-<left>" . tab-previous)
  ("s-k" . tab-next)
  ("s-j" . tab-previous)
  ;; Turn on tab-bar-mode in early-init to speed-up
  ;; :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 30
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-tab-hints t
        tab-bar-show 1)

  ;; init time 的罪魁祸首
  ;; (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

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
                                               ;; +tab-bar-persp-indicator
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
  ("0" tab-close)
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
