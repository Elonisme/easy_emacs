;;; init-themes.el -- theme settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package color-theme-sanityinc-tomorrow
  :ensure t
  )

;; 根据时间自动切换主题
(defun my/set-theme-based-on-time ()
  "自动切换主题：白天使用亮色主题，晚上使用暗色主题"
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (and (>= hour 7) (< hour 19))
        (load-theme 'sanityinc-tomorrow-day t)  ;; 白天使用亮色主题
      (load-theme 'sanityinc-tomorrow-night t)))) ;; 晚上使用暗色主题

;; 启动时设置初始主题
(my/set-theme-based-on-time)

;; 每小时检查一次并切换主题
(run-with-timer 0 (* 60 60) 'my/set-theme-based-on-time)

(provide 'init-themes)

;;; init-themes.el ends here
