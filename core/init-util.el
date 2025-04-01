;;; -*- lexical-binding: t -*-

;;; 工具包
;; 更加方便的快捷键设置，放在所有包之前
(use-package transient
  :ensure t)

;; 规范化 emacs.d 的结构，使得配置集中于 ~/.emacs.d/etc，临时数据集中于 ~/.emacs.d/var
(use-package no-littering
  :ensure t
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")) ; 设置 custom-file 路径
  )

;; 使用 M-x esup 就可以显示关键路径
(use-package esup
  :ensure t
  :config
  ;; 需要修改这个函数才能确保正常工作
  (defun my-esup-read-results-advice (orig-fun &rest args)
    "My custom advice for `esup-read-results`."
    (let (results sep-end-point)
      (with-current-buffer (get-buffer esup-incoming-results-buffer)
        (goto-char esup-last-result-start-point)
        (message "at %s" esup-last-result-start-point)
        (unless (eobp)
          (while (setq sep-end-point (esup-next-separator-end-point))
            (when-let ((result (car (esup-read-result (point)))))
              (push result results))
            (setq esup-last-result-start-point sep-end-point)
            (goto-char esup-last-result-start-point))))
      (nreverse results)))

  )

;; 展示颜色，用于 CSS 或者 HTML，当有 256 进制颜色的时候，可以 overlay 出颜色
(use-package rainbow-mode
  :ensure t
  :functions (rainbow-turn-off rainbow-colorize-match rainbow-x-color-luminance)
  :bind (:map help-mode-map
              ("r" . rainbow-mode))
  :hook ((html-mode css-mode) . rainbow-mode)
  )

;; 单独设置字体
(defun +set-buffer-face-mode-mono ()
  "Set buffer-face-mode for org-agenda."
  (interactive)
  (setq buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode))

;;; 工具宏
;; Thanks to DOOM Emacs
(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The hook(s) to add to.
  2. REST Optional properties :local, :append, and/or :depth [N].
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N] :remove :call-immediately] FUNCTIONS-OR-FORMS...)"
  (declare (indent defun))
  (let* ((hook-forms (if (listp hooks) hooks (list hooks)))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p call-immediately-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))
        (:call-immediately (setq call-immediately-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil)) next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (func (list ,@func-forms))
         (dolist (hook (nreverse ',hook-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))
         ,(if call-immediately-p `(funcall func))))))

(defmacro defadvice! (symbol arglist &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &rest [WHERE PLACES...] BODY\)"
  (declare (indent defun))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro defun-call! (symbol args &rest body)
  "Define a function and optionally apply it with specified arguments.

\(fn SYMBOL ARGS &optional [DOCSTRING] &optional [:call-with APPLY_ARGS] BODY\)"
  (declare (indent defun))
  (let* ((docstring (if (stringp (car body)) (pop body)))
         (apply-body (if (eq :call-with (car body))
                         (progn
                           (cl-assert (eq (pop body) :call-with))
                           (pop body))
                       nil)))
    `(progn
       (defun ,symbol ,args
         ,@(if docstring
               (cons docstring body)
             body))
       (apply ',symbol
              ,(if (listp apply-body)
                   `(list ,@apply-body)
                 `(list ,apply-body))))))
