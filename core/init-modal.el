;;; -*- lexical-binding: t; -*-

;;; NOTE:
;;; 没有改到 hjkl 键位的关键在于，minibuffer 是没有办法用 C-j, C-k 的
;;; 所以会造成一定的不一致性。
;;;
;;; keypad 模式的逻辑是这样的，当按下 SPC 后，按 c, x, h 会分别对应 C-c, C-x, C-h
;;; 之后再按的字母 <c> 都会变成 C-<c> ，如果想按出来不加 C- 的，那么就需要按 SPC-<c>
;;; 此外 keypad 还有 fallback 逻辑，指的是当无法匹配的时候，会采用一种退而求其次的匹配
;;; 但是 fallback 具体是什么我理解不了，我个人感觉是 SPC 后加一个非 c, x, h 键 <c>
;;; 那么就会被翻译成 C-c <c> 。
;;; 这种机制会导致基本上都是 3 键才能解决形如 C-[cx] C-<c> 的按键，这相对于原来 2 键就解决耽误了不少。
;;; 对于 C-x <c> 需要 4 键才能解决，这些都是不能接受的。
;;; 所以要将这些按键尽量放到 normal 下。
;;; 但是 C-c C-<c> 因为是 major-mode 相关，所以没法简单改键位，所以这种按键会非常麻烦
;;;
;;; 在 motion 模式时，按键基本上就是原生的，对于 dired, magit 这样的特殊模式本身没有关系
;;; 但是在切换 window 的时候，原本 normal 熟悉的按键不能使用了，只能用 keypad 模式按键
;;; 而 window 切换又是非常难按的 4 键，所以要特殊设计

(use-package meow
  :straight t
  :hook
  (after-init . meow-global-mode)
  ;; 改变 meow 的默认区域删除行为
  (meow-global-mode . (lambda () (setq delete-active-region t)))
  ;; (meow-insert-enter . (lambda () (corfu-mode +1)))
  :demand t
  :custom
  (meow-keypad-meta-prefix ?z)
  (meow-keypad-ctrl-meta-prefix ?Z)
  (meow-keypad-describe-delay 1.0)
  :config
  ;; 关闭 change 选择行为
  (setq meow-select-on-change nil)
  ;; 方便使用 SPC j b 打开 buffer
  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  ;; 修复 meow 的原生功能。
  (defmacro +meow-amend-keybinding (key name command)
    (let ((kbd-symbol (intern (format "meow--kbd-%s" name))))
      `(progn
         (global-set-key (kbd (concat "H-" ,key)) ',command)
         (setq ,kbd-symbol (concat "H-" ,key)))))

  (+meow-amend-keybinding "C-d" delete-char delete-char)
  (+meow-amend-keybinding "C-/" undo undo)
  (+meow-amend-keybinding "C-w" kill-ring-save kill-ring-save)
  (+meow-amend-keybinding "M-w" kill-region kill-region)
  (+meow-amend-keybinding "M-;" comment comment-dwin)
  ;; (+meow-amend-keybinding "C-f" forward-char forward-char)
  (+meow-amend-keybinding "C-k" kill-line kill-line)

  ;; 在行尾进入 insert 模式
  (defun +meow-append-line ()
    (interactive)
    (progn
      (end-of-line)
      (meow-append)))
  (defalias '+mark-deactive
    (kmacro "C-SPC C-SPC"))
  ;; [motion]
  (meow-motion-overwrite-define-key
   ;; '("j" . meow-next)
   ;; '("k" . meow-prev)
   '("<escape>" . ignore))
  ;; [leader]
  (meow-leader-define-key
   ;; 将 j 视为 x
   '("j" . "C-x")
   ;; Use SPC (0-9) for digit arguments.
   ;; '("1" . meow-digit-argument)
   ;; '("2" . meow-digit-argument)
   ;; '("3" . meow-digit-argument)
   ;; '("4" . meow-digit-argument)
   ;; '("5" . meow-digit-argument)
   ;; '("6" . meow-digit-argument)
   ;; '("7" . meow-digit-argument)
   ;; '("8" . meow-digit-argument)
   ;; '("9" . meow-digit-argument)
   ;; '("0" . meow-digit-argument)
   '("o" . ace-window)
   '("O" . org-capture)
   '("9" . ace-delete-window)
   '("8" . ace-swap-window)
   '("0" . delete-window)
   '("1" . delete-other-windows)
   '("2" . split-window-below)
   '("3" . split-window-right)
   '("4 f" . find-file-other-window)
   '("4 b" . switch-to-buffer-other-window)
   '("R" . Cx-r/body)
   '("T" . window/body)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   )

  ;; [normal]
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)

   ;; 模式切换键
   '("I" . meow-open-below)
   '("i" . meow-append)
   '("c" . meow-change)
   '("E" . +meow-append-line)

   ;; move
   '("<" . beginend-prog-mode-goto-beginning)
   '(">" . beginend-prog-mode-goto-end)
   '("f" . "C-f")
   '("F" . meow-right-expand)
   '("b" . backward-char)
   '("B" . meow-left-expand)
   '("n" . next-line)
   '("N" . meow-next-expand)
   '("p" . previous-line)
   '("P" . meow-prev-expand)
   '("a" . move-beginning-of-line)
   '("e" . move-end-of-line)
   '("o" . hs-toggle-hiding)
   '("h" . meow-find)
   ;; '("j" . ace-pinyin-goto-char-timer)

   ;; 剪切，复制，粘贴，注释
   '("q" . my-kill-region-or-line)
   '("w" . my-copy-region-or-line)
   '("y" . yank)
   '("Y" . yank-media)
   '("/" . +smart-comment)
   '("." . embrace-commander)
   '("k" . puni-kill-line)

   ;; 撤销
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("x" . meow-quit)

   ;; 搜索
   '("S" . +consult-ripgrep-single-file)
   '("s" . consult-line)
   '("r" . query-replace-regexp)

   ;; 标记与区域
   '("v" . set-mark-command)
   '("V" . exchange-point-and-mark)
   '(";" . embark-act)
   '("M" . pop-to-mark-command)
   '("l" . er/expand-region)
   '("L" . er/contract-region)
   '("g" . meow-cancel-selection)
   ;; 多光标
   ;; grab 会将原来的 region 当成一个 grab region 在这里可以创造多个光标。
   ;; 默认是每一行创造一个光标
   ;; 也可以是使用 next-word 或者 find 指令主动创造行内光标
   ;; grab 的本质是第二选区，可以和第一选区互相作用
   '("G" . meow-grab)
   '("W" . meow-next-word)
   ;; 其他
   '("," . sdcv-search-pointer+)
   '("d" . gptel)
   '("t" . +start-vterm-in-project)
   )
  ;; 在退出 insert 模式时自动退出 corfu 补全
  (with-eval-after-load 'corfu
    (when (fboundp 'corfu-quit)
      (add-hook 'meow-insert-exit-hook 'corfu-quit)))

  (with-eval-after-load 'eaf-pdf-viewer
    (eaf-bind-key meow-keypad "<SPC>" eaf-pdf-viewer-keybinding))
  (dolist
      (state
       '(
         (color-rg-mode . motion)
         ))
    (add-to-list 'meow-mode-state-list state))
  )

;; vterm 会与 meow normal 键冲突
;; 使得 normal-mode 键绑定都可以正常使用
(use-package meow-vterm
  :straight (meow-vterm :type git :host github :repo "accelbread/meow-vterm")
  :after vterm
  :init
  (setq vterm-keymap-exceptions '("C-c"))
  (meow-vterm-enable))

;; change the cursor color with the input-method changing
(defvar cursor-default-color (face-background 'cursor))
(defvar cursor-activate-color (face-foreground 'error nil t))

(defun set-cursor-color-red ()
  "Set the cursor color to red."
  (set-cursor-color cursor-activate-color))
(defun set-cursor-color-default ()
  "Set the cursor color to green."
  (set-cursor-color cursor-default-color))
(defun set-cursor-color-according-to-input-method ()
  "Set cursor color based on the current input method."
  (interactive)
  (if (string= current-input-method "rime")
      (set-cursor-color cursor-activate-color)
    (set-cursor-color cursor-default-color)))

(add-hook 'input-method-activate-hook 'set-cursor-color-red)
(add-hook 'input-method-deactivate-hook 'set-cursor-color-default)
(add-hook 'window-state-change-hook 'set-cursor-color-according-to-input-method)
