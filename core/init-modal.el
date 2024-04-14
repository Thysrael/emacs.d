;;; -*- lexical-binding: t; -*-

(use-package meow
  :straight t
  :hook
  (after-init . meow-global-mode)
  (meow-global-mode . (lambda () (setq delete-active-region t)))
  :demand t
  :config
  (setq meow-expand-hint-remove-delay 3.0)
  (defmacro +meow-amend-keybinding (key name command)
    (let ((kbd-symbol (intern (format "meow--kbd-%s" name))))
      `(progn
         (global-set-key (kbd (concat "<f9> " ,key)) ',command)
         (setq ,kbd-symbol (concat "<f9> " ,key)))))

  (+meow-amend-keybinding "C-d" delete-char delete-char)
  (+meow-amend-keybinding "C-/" undo undo)
  (+meow-amend-keybinding "C-w" kill-ring-save kill-ring-save)
  (+meow-amend-keybinding "M-w" kill-region kill-region)
  (+meow-amend-keybinding "M-;" comment comment-dwin)
  (+meow-amend-keybinding "C-f" forward-char forward-char)
  (+meow-amend-keybinding "C-k" kill-line kill-line)
  ;; [motion]
  (meow-motion-overwrite-define-key
   '("<escape>" . ignore))
  ;; [leader]
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "C-x")
   '("b" . switch-to-buffer)
   '("B" . ibuffer)
   '("t" . "C-t")
   '("T" . multi-vterm)
   '("R" . "C-x r")
   '("S" . save-buffer)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  ;; [normal]
  (meow-normal-define-key
   '("<" . beginend-prog-mode-goto-beginning)
   '(">" . beginend-prog-mode-goto-end)
   ;; 模式切换键
   '("O" . meow-open-below)
   '("i" . meow-append)
   '("I" . meow-open-above)
   '("c" . meow-change)
   ;; 移动键
   ;; 简单移动键
   '("f" . +smart-forward)
   '("F" . meow-right-expand)
   '("b" . backward-char)
   '("B" . meow-left-expand)
   '("n" . next-line)
   '("N" . meow-next-expand)
   '("p" . previous-line)
   '("P" . meow-prev-expand)
   ;; 单词符号移动键，可以移动到单词的行尾或者行首
   ;; '("w" . meow-next-word)
   ;; '("W" . meow-next-symbol)
   ;; 段首尾移动
   '("a" . move-beginning-of-line)
   '("e" . mosey-forward-cycle)

   ;; 剪切，复制，粘贴，注释
   '("q" . my-kill-region-or-line)
   '("w" . my-copy-region-or-line)
   '("y" . yank)
   '("k" . puni-kill-line)
   '("g" . meow-cancel-selection)
   '("l" . er/expand-region)
   '("L" . er/contract-region)
   '("/" . +smart-comment)
   '("." . embrace-commander)
   ;; 撤销，退出
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   ;; 搜索
   '("s" . consult-line)
   '("r" . query-replace-regexp)
   ;; window
   '("o" . ace-window)
   '("9" . ace-delete-window)
   '("8" . ace-swap-window)
   '("0" . delete-window)
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("3" . split-window-right)
   '("4 f" . find-file-other-window)
   '("4 b" . switch-to-buffer-other-window)
   ;; 标记
   '("m" . set-mark-command)
   '("M" . pop-to-mark-command)
   ;; 多光标
   ;; grab 会将原来的 region 当成一个 grab region 在这里可以创造多个光标。
   ;; 默认是每一行创造一个光标
   ;; 也可以是使用 next-word 或者 find 指令主动创造行内光标
   '("G" . meow-grab)
   '("W" . meow-next-word)
   '("H" . meow-find)
   ;; 其他
   '("h" . avy-goto-char-2)
   '(";" . embark-act)
   '("," . sdcv-search-pointer+)
   '("d" . chatgpt-shell)
   '("t" . +start-vterm-in-project)
   '("-" . popper-cycle)
   '("=" . popper-toggle-type)
   )

  (dolist
      (state
       '(
         (View-mode . normal)
         (fundamental-mode . normal)
         (vterm-mode . insert)
         (vundo-mode . motion)
         (chatgpt-shell-mode . insert)
         ))
    (add-to-list 'meow-mode-state-list state))
  )
