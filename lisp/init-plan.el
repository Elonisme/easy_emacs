;;; init-plan --- 时间管理
;;; commentary:
;;; Code:

(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/org/tasks.org"
                           "~/org/schedule.org"))
  (global-set-key (kbd "C-c a") 'org-agenda))


(use-package org-capture
  :after org
  :config
  (setq org-capture-templates
        '(("t" "任务" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %?\n  %U\n")
          ("n" "笔记" entry (file+datetree "~/org/notes.org")
           "* %?\n  %U\n")))
  (global-set-key (kbd "C-c c") 'org-capture))


(use-package org-clock
  :after org
  :config
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate))

(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-x") 'org-clock-cancel)
(global-set-key (kbd "C-c C-x C-d") 'org-clock-display)

(provide 'init-plan)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-plan.el ends here
