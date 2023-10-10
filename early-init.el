;;; -*- lexical-binding: t -*-
;;; 主要用于提高启动速度

;; 内存垃圾回收优化
(setq gc-cons-threshold most-positive-fixnum  ; 垃圾回收的上限增大
      gc-cons-percentage 1.0)   ; 每次垃圾回收时的比例

;; 开启延迟原生编译
(setq native-comp-deferred-compilation t)

;; emacs 非交互式对话加载时更加倾向加载 el 文件而非 elc, eln 文件，据说可以节约 IO 时间
;; 这是因为一般的加载会优先考虑 elc 和 eln
(setq load-prefer-newer noninteractive)

;; 禁止自动调整 Emacs 窗口框架的大小
(setq frame-inhibit-implied-resize t)

;; 禁用起始界面
(setq inhibit-startup-screen t  ; 禁用起始画面
      inhibit-startup-echo-area-message t ; 禁用起始时回显区域
      inhibit-startup-message t ; 禁用起始信息
      initial-scratch-message nil ; 清除 scratch 注释信息
      initial-major-mode 'fundamental-mode) ; 设置起始主模式
(advice-add #'display-startup-echo-area-message :override #'ignore) ; 将绘制起始信息函数清空
(advice-add #'display-startup-screen :override #'ignore) ; 将绘制起始画面函数清空

;; 防止在窗口初始化期间出现屏幕闪烁
(setq inhibit-redisplay t
      inhibit-message t)    ; 首先禁止屏幕闪烁
(add-hook 'window-setup-hook
          (lambda ()
            (setq inhibit-redisplay nil
                  inhibit-message nil)
            (redraw-frame)))    ;; 在窗口初始化完成后重新开启这个功能

;; 禁用 package.el，因为我们会改用更加先进的包管理器
(setq package-enable-at-startup nil)

;; 禁用菜单栏工具栏等元素
(push '(menu-bar-lines . 0) default-frame-alist)    ; 在 default-frame 的元素中取消 menu-bar
(push '(tool-bar-lines . 0) default-frame-alist)    ; 在 default-frame 的元素中取消 tool-bar
(push '(vertical-scroll-bars) default-frame-alist)  ; 在 default-frame 的元素中取消 vertical-scroll-bar
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)  ; 再关闭一下，避免出事

;; 使文件大小写敏感，可以节省时间，据说是分析关键路径得出的结论
(setq auto-mode-case-fold nil)

;; 禁用文件名管理器，可以提高文件加载速度
(unless (or (daemonp) noninteractive init-file-debug) ; 只有在这三种情况下不禁用
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)  ; 禁用
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-value))))))) ; 重新恢复
;; 禁止显示加载信息
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))
(define-advice startup--load-user-init-file (:after (&rest _) undo-silence)
  (advice-remove #'load-file #'load-file@silence))

