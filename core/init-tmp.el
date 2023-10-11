;; (use-package holo-layer
;;   :straight (holo-layer :type git :host github :repo "manateelazycat/holo-layer"))

;; 果冻光标
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/holo-layer")
;; (require 'holo-layer)
;; (setq holo-layer-enable-cursor-animation t)
;; (holo-layer-enable)




;; (use-package key-echo
;;   :straight (key-echo :type git :host github :repo "manateelazycat/key-echo"))

(add-to-list 'load-path "~/.emacs.d/straight/repos/key-echo/")
(require 'key-echo)
(setq key-echo-enable-debug t)

(key-echo-enable)

(defun key-echo-shift-to-switch-input-method (key)
  (interactive)
  (when (string-equal key "Key.shift")
    (toggle-input-method)
    ))

(setq key-echo-single-key-trigger-func 'key-echo-shift-to-switch-input-method)

