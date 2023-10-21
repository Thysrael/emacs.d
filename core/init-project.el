;;; -*- lexical-binding: t -*-

;;; 项目管理
(use-package project
  :bind
  ("C-x p" . hydra-project/body)
  :config
  (defhydra hydra-project (:color blue
                        :hint nil
                        :columns 5)
  "Project Commands"
  ("b" project-list-buffers "List Buffers" :color blue)
  ("x" project-execute-extended-command "Execute Extended Command" :color blue)
  ("r" project-query-replace-regexp "Query Replace Regexp" :color blue)
  ("G" project-or-external-find-regexp "Find Regexp (External)" :color blue)
  ("g" project-find-regexp "Find Regexp (Project)" :color blue)
  ("p" project-switch-project "Switch Project" :color blue)
  ("k" project-kill-buffers "Kill Buffers" :color blue)
  ("e" project-eshell "Eshell" :color blue)
  ("c" project-compile "Compile" :color blue)
  ("v" project-vc-dir "Version Control (VC) Dir" :color blue)
  ("D" project-dired "Dired" :color blue)
  ("d" project-find-dir "Find Directory" :color blue)
  ("s" project-shell "Shell" :color blue)
  ("S" project-switch-to-buffer "Switch to Buffer" :color blue)
  ("F" project-or-external-find-file "Find File (External)" :color blue)
  ("f" project-find-file "Find File" :color blue)
  ("&" project-async-shell-command "Async Shell Command" :color blue)
  ("!" project-shell-command "Shell Command" :color blue))
)
