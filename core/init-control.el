;;; -*- lexical-binding: t -*-

(use-package org
  :config
  ;; WORKAROUND: I don't know why I can't set it in :custom
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path nil)
  ;; 强制 TODO 的状态切换依赖（父任务必须等子任务完成才能完成）
  (setq org-enforce-todo-dependencies t)
  ;; ;; 设置父任务自动完成，当子任务全部完成
  ;; (defun org-summary-todo (n-done n-not-done)
  ;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
  ;;   (let (org-log-done org-log-states) ; turn off logging
  ;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  ;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  :init
  ;; 禁止日程启动画面
  (setq org-agenda-inhibit-startup t)
  :custom
  ;; 当状态从 DONE 改成其他状态时，移除 CLOSED: [timestamp]
  (org-closed-keep-when-no-todo t)
  ;; ;; DONE 时加上时间戳
  ;; (org-log-done 'time)
  ;; ;; 重复执行时加上时间戳
  ;; (org-log-repeat 'time)
  ;; Deadline 修改时加上一条记录
  (org-log-redeadline 'note)
  ;; Schedule 修改时加上一条记录
  (org-log-reschedule 'note)
  ;; 以抽屉的方式记录
  (org-log-into-drawer t)
  ;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
  (org-log-state-notes-insert-after-drawers nil)

  ;; refile 使用缓存
  (org-refile-use-cache nil)
  ;; refile 的目的地，这里设置的是 agenda 文件的所有标题
  (org-refile-targets '((org-agenda-files . (:maxlevel . 1))))
  ;; 是否按步骤 refile
  (org-outline-path-complete-in-steps nil)
  ;; 将文件名加入到路径
  (org-refile-use-outline-path nil)
  ;; 允许创建新的标题行，但需要确认
  (org-refile-allow-creating-parent-nodes 'confirm)
  ;; 设置标签的默认位置，默认是第 77 列右对齐
  (org-tags-column -80)
  ;; 自动对齐标签
  (org-auto-align-tags nil)
  ;; 标签不继承
  (org-use-tag-inheritance nil)
  ;; 在日程视图的标签不继承
  (org-agenda-use-tag-inheritance nil)
  ;; 标签快速选择
  (org-use-fast-tag-selection t)
  ;; 标签选择不需要回车确认
  (org-fast-tag-selection-single-key t)
  ;; 定义了有序属性的标题行也加上 OREDERD 标签
  (org-track-ordered-property-with-tag t)

  ;; 预定义好的标签
  (org-tag-alist '((:startgroup)
                   ("read"     . ?r)
				   ("emacs"    . ?e)
				   ("study"    . ?s)
				   ("work"     . ?w)
				   ("blog"     . ?b)
				   ("linux"    . ?l)
				   ("misc"     . ?m)
				   (:endgroup)))
  ;; 预定义好的 seq
  (org-todo-keywords
   '((sequence "TODO(t!)" "PEND(p@/!)" "|" "DONE(d!)" "ABRT(a@/!)")))
  )

(use-package org-capture
  :straight nil
  :bind
  ("C-c o" . org-capture)
  :config
  (defun +org-get-top-headings ()
    "Get the names of the headings in the current org file."
    (save-excursion
      (goto-char (point-min))
      (let ((headings nil))

        (while (re-search-forward "^[\\*]+ \\([^\\[\n]+\\)" nil t)
          (let ((heading (match-string-no-properties 1)))
            (add-to-list 'headings heading t)))
        headings)))

  (defun +org-select-top-level-header ()
    "Visit all headings in the current file."
    (interactive)
    (goto-char (point-min))
    (let ((choice (completing-read "Heading: " (+org-get-top-headings))))
      (goto-char (point-min))
      (re-search-forward (format "^[\\*]+ %s" choice))
      (forward-line 1)))
  (defun prio ()
    (format "[#%c]" org-default-priority))
  :custom
  (org-default-notes-file "~/learn/org/agenda.org")
  ;; %^g: tag
  ;; %U: unactivated timestamp
  ;; %^{prompt}: input content
  ;; %^<something> will call the virtico
  ;; :empty-lines-after 1  这个是在这个快速记录后插入一个空行
  (org-capture-templates
   '(
     ("i" "Inbox" entry
      (file+olp "" "inbox")
      "* TODO %(prio) %? %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines-after 1)
     ("s" "Schedule" entry
      (file+olp "" "schedule")
      "* TODO %? %^g\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines-after 1)
     ("d" "Deadline" entry
      (file+olp "" "inbox")
      "* TODO %? %^g\nDEADLINE: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines-after 1)
     ("S" "Someday" entry
      (file+olp "" "someday")
      "* TODO %(prio) %? %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines-after 1)
     ("r" "References" entry
      (file+olp "" "references")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%c\n"
      :empty-lines-after 1)
     ("a" "Append" item
      (file+function "" +org-select-top-level-header)
      "%?")
     ("h" "Habit" entry
      (file+olp "" "habits")
	  "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a ++1d>>\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:END:\n"
      :empty-lines-after 1)
     ))
  ;; :config
  ;; (add-to-list 'org-capture-templates
  ;;              '("w" "Word" entry
  ;;                (file+olp "~/learn/org/words.org" "Inbox")
  ;;                "* TODO %^{Word}\n%?"))
  )

(use-package org-agenda
  :straight nil
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files
   '("~/learn/org/agenda.org"))
  ;; 日视图还是周视图，通过 v-d, v-w, v-m, v-y 切换视图，默认周视图
  (org-agenda-span 'day)
  ;; q 退出时删除 agenda 缓冲区
  (org-agenda-sticky t)
  ;; 是否包含直接日期
  (org-agenda-include-deadlines t)
  ;; 显示每一天，不管有没有条目
  (org-agenda-show-all-dates t)
  ;; 时间不足位时前面加 0
  (org-agenda-time-leading-zero t)
  ;; 日程同时启动 log mode
  (org-agenda-start-with-log-mode t)
  ;; 日程同时启动任务时间记录报告模式
  (org-agenda-start-with-clockreport-mode t)
  ;; 从星期一开始作为一周第一天
  (org-agenda-start-on-weekday 1)
  ;; 是否使用am/pm
  ;; (org-agenda-timegrid-use-ampm nil)
  ;; 搜索是不看时间
  (org-agenda-search-headline-for-time nil)
  ;; 提前 3 天截止日期到期告警
  (org-deadline-warning-days 3)
  ;; clock report level 增加到 3
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  ;; :custom-face
  ;; (org-schedule ((t (:family "Sarasa Mono SC"))))
  ;; (org-scheduled ((t (:family "Sarasa Mono SC"))))
  ;; (org-scheduled-today ((t (:family "Sarasa Mono SC"))))
  ;; (org-scheduled-previously ((t (:family "Sarasa Mono SC"))))
  :hook
  (org-agenda-mode . +set-buffer-face-mode-mono)
  :custom-face
  (org-scheduled-today ((t (:foreground "#f1fa8c"))))
  ;; (org-scheduled ((t (:foreground "#ffb86c"))))
  )

;; 在 agenda 上显示记录
;; ! means not done. * means done.
;; future 实际指的是“非本天”的意思，也就是如果不在当天这一列，那么就是非本天
;; alert: 当天应当做的事情 alert-future 某天应当做的事情
;; clear: 当天不需要做的事情 clear-future 某天不需要做的事情
;; overdue: 当天已经过期的事情 overdue-future 某天已经过期的事情
;; 需要注意 overdue 是相对于今天特定时间的事情，比如说今天的事情相对于明天来说一定是过期的
;; ready: 当天已经做完的事情
(use-package org-habit
  :straight nil
  :after org-agenda
  :init
  (require 'org-habit)
  :custom
  (org-habit-show-habits t)
  (org-habit-graph-column 50)
  (org-habit-show-all-today t)
  (org-habit-show-done-always-green t)
  ;; (org-habit-scheduled-past-days t)
  ;; ;; org habit show 7 days before today and 7 days after today.
  (org-habit-preceding-days 4)
  :custom-face
  ;; (org-habit-alert-face ((t (:background "#ffc107" :weight bold))))
  ;; (org-habit-alert-future-face ((t (:background "#ffa000" :weight bold))))
  ;; (org-habit-overdue-face ((t (:background "#e64a19" :weight bold))))
  ;; (org-habit-overdue-future-face ((t (:background "#bf360c" :weight bold))))
  ;; (org-habit-clear-face ((t (:background "#009688" :weight bold))))
  ;; (org-habit-clear-future-face ((t (:background "#00796B" :weight bold))))
  ;; (org-habit-ready-face ((t (:background "#689F38" :weight bold))))
  ;; (org-habit-ready-future-face ((t (:background "#CDDC39" :weight bold))))

  (org-habit-alert-face ((t (:background "#edd389" :weight bold))))
  (org-habit-alert-future-face ((t (:background "#d0bf8f" :weight bold))))
  (org-habit-overdue-face ((t (:background "#8b3c3c" :weight bold))))
  (org-habit-overdue-future-face ((t (:background "#8c5353" :weight bold))))
  (org-habit-clear-face ((t (:background "#418d93" :weight bold))))
  (org-habit-clear-future-face ((t (:background "#4c7073" :weight bold))))
  (org-habit-ready-face ((t (:background "#7f9f7f" :weight bold))))
  (org-habit-ready-future-face ((t (:background "#5f7f5f" :weight bold))))
  )

(use-package calendar
  :straight nil
  :bind
  ("C-c A" . calendar)
  :hook (calendar-today-visible . calendar-mark-today)
  :custom
  ;; 是否显示中国节日，我们使用 `cal-chinese-x' 插件
  (calendar-chinese-all-holidays-flag nil)
  ;; 是否显示节日
  (calendar-mark-holidays-flag t)
  ;; 是否显示 Emacs 的日记，我们使用 org 的日记
  (calendar-mark-diary-entries-flag nil)
  ;; 数字方式显示时区，如 +0800，默认是字符方式如 CST
  (calendar-time-zone-style 'numeric)
  ;; 日期显示方式：year/month/day
  (calendar-date-style 'iso)
  ;; 中文天干地支设置
  (calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; 周一作为一周第一天
  (calendar-week-start-day 1)
  )

;; 时间解析增加中文拼音
(use-package parse-time
  :ensure nil
  :defer t
  :config
  (setq parse-time-months
        (append '(("yiy" . 1) ("ery" . 2) ("sany" . 3)
                  ("siy" . 4) ("wuy" . 5) ("liuy" . 6)
                  ("qiy" . 7) ("bay" . 8) ("jiuy" . 9)
                  ("shiy" . 10) ("shiyiy" . 11) ("shiery" . 12)
                  ("yiyue" . 1) ("eryue" . 2) ("sanyue" . 3)
                  ("siyue" . 4) ("wuyue" . 5) ("liuyue" . 6)
                  ("qiyue" . 7) ("bayue" . 8) ("jiuyue" . 9)
                  ("shiyue" . 10) ("shiyiyue" . 11) ("shieryue" . 12))
                parse-time-months))

  (setq parse-time-weekdays
        (append '(("zri" . 0) ("zqi" . 0)
                  ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                  ("zsi" . 4) ("zwu" . 5) ("zliu" . 6)
                  ("zr" . 0) ("zq" . 0)
                  ("zy" . 1) ("ze" . 2) ("zs" . 3)
                  ("zsi" . 4) ("zw" . 5) ("zl" . 6))
                parse-time-weekdays)))

;; 中国节日设置
(use-package cal-china-x
  :straight t
  :commands cal-china-x-setup
  :hook (after-init . cal-china-x-setup)
  :custom-face
  (cal-china-x-important-holiday-face ((t (:background "#ff757f"))))
  (cal-china-x-general-holiday-face ((t (:background "#82aaff"))))
  :config
  ;; 重要节日设置
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  ;; 所有节日设置
  (setq cal-china-x-general-holidays
        '(;;公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 14 "白色情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-fixed 5 4 "青年节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          (holiday-fixed 10 2 "国庆节")
          (holiday-fixed 10 3 "国庆节")
          (holiday-fixed 10 24 "程序员节")
          (holiday-fixed 12 25 "圣诞节")
          ;; 农历节日
          (holiday-lunar 12 30 "春节" 0)
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 2 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          (holiday-lunar 7 7 "七夕情人节" 0)
          (holiday-lunar 12 8 "腊八节" 0)
          (holiday-lunar 9 9 "重阳节" 0)))
  ;; 设置日历的节日，通用节日已经包含了所有节日
  (setq calendar-holidays (append cal-china-x-general-holidays))
  )
