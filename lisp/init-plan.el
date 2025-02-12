;;; init-plan --- 时间管理
;;; commentary:
;;; Code:

(use-package org
  :ensure t
  :config
  ;; 配置 org-agenda 文件
  (setq org-agenda-files '("~/org/tasks.org"
                           "~/org/schedule.org"))
  ;; 设置快捷键
  (global-set-key (kbd "C-c a") 'org-agenda))



(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        '(("t" "任务" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %?\n  %U\n")
          ("s" "日程" entry (file+headline "~/org/schedule.org" "Schedule")
           "* TODO %?\n  SCHEDULED: %^{Scheduled Time|<%(format-time-string \"%Y-%m-%d %H:%M\")>}T\n  %U\n" :empty-lines 1)
          ("n" "闪念笔记" entry (file+datetree "~/org/notes.org")
           "* %?\n  %U\n")))

  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

  (setq org-agenda-prefix-format
        '((agenda . " %-12:c%?-12t %s") 
          (todo   . " %-12:c")          
          (tags   . " %-12:c")          
          (search . " %-12:c")))        

  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda))


(use-package org-clock
  :after org
  :config
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate))

(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-x") 'org-clock-cancel)
(global-set-key (kbd "C-c C-x C-d") 'org-clock-display)

(use-package org-alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 3000)  ;; 每 30 分钟检查一次
  (org-alert-enable))


(provide 'init-plan)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-plan.el ends here
