;;; -*- lexical-binding: t -*-

;; 设置代理
;; (setq url-proxy-services '(("no_proxy" . "^\\(192\\.168\\..*\\)")
;;                            ("http" . "127.0.0.1:7897")
;;                            ("https" . "127.0.0.1:7897")))

(setq comp-deferred-compilation-deny-list ()
      warning-suppress-log-types '((comp)))

(require 'package)
(package-initialize)
;; add more archive: gnu, nongnu, melpa
;; gnu elpha is offical but small
;; melpha is large but unoffical
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-vc-allow-build-commands t)

;; :vc shadow clone
(defun my/vc-git-clone (fn remote directory rev)
  (if (or (not (string-match-p "elpa" directory))
          (null rev))
      (funcall fn remote directory rev)
    (cond
     ((ignore-errors
        ;; First try if rev is a branch/tag name
        ;; https://stackoverflow.com/a/48748567/2163429
        (vc-git--out-ok "clone" "--depth" "1" "--single-branch" "--branch" rev remote directory)))
     ((vc-git--out-ok "clone" "--single-branch" remote directory)
      (let ((default-directory directory))
        (vc-git--out-ok "checkout" rev))))
    directory))

(advice-add 'vc-git-clone :around 'my/vc-git-clone)

;; use-packge 的设置
(setq use-package-always-demand (daemonp) ; 当 Emacs 是以守护进程方式运行时，立即加载所需的包
      use-package-always-defer (not (daemonp)) ; 当 Emacs 不是以守护进程方式运行时，延迟加载所需的包
      use-package-expand-minimally t ; 在展开配置时尽可能地精简，以减少额外的加载和计算
      use-package-enable-imenu-support t ; 使得可以在 buffer 中快速导航到 use-package 定义的位置
      )

;; (setq straight-check-for-modifications nil                   ; skip modification
;;       straight-vc-git-default-clone-depth '(1 single-branch) ; shadow clone
;;       comp-deferred-compilation-deny-list ()                 ; config native comp
;;       warning-suppress-log-types '((comp))                   ; Don't display comp warnings
;;       straight-disable-native-compile (not (and (fboundp 'native-comp-available-p)
;;                                                 (native-comp-available-p))))

;; ;; 设置 protocol 协议为 ssh ，可以避免 https 连不上时的卡死
;; (setq straight-vc-git-default-protocol 'ssh)
;;
;; ;; 安装 straight.el，我们会使用这个新包管理器，但是需要手动下载
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; use-package 的安装
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
