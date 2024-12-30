;;; init-themes.el -- theme settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(use-package color-theme-sanityinc-tomorrow
  :ensure t
  )

(use-package color-theme-sanityinc-solarized
  :ensure t
  )
(load-theme 'sanityinc-solarized-light t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)  ;; 启动时显示 dashboard
  (setq dashboard-banner-logo-title "Welcome to Emacs!"
        dashboard-startup-banner 'official  ;; 可以选择 logo 图片路径
        dashboard-center-content t
        dashboard-items '((recents  . 10)    ;; 最近打开的文件
                          (bookmarks . 5)   ;; 书签
                          (projects . 5))   ;; 最近的项目
        dashboard-set-footer nil))        ;; 关闭底部的 footer
(dashboard-open t)


(provide 'init-themes)
;;; init-themes.el ends here
