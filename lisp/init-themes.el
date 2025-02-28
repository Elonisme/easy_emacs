;;; init-themes.el -- theme settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; 安装 all-the-icons
(use-package all-the-icons
  :ensure t)

;; 安装 nerd-icons
(use-package nerd-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package color-theme-sanityinc-solarized
  :ensure t
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)  ;; 启动时显示 dashboard
  (setq dashboard-banner-logo-title "Welcome to Emacs!"
        dashboard-startup-banner "~/.emacs.d/logo.png"  ;; 可以选择 logo 图片路径
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-items '((recents . 5)    ;; 最近打开的文件
                          (bookmarks . 5)   ;; 书签
                          (agenda . 5)      ;; agenda
                          (projects . 5))   ;; 最近的项目
        )

  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-modify-heading-icons '((recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-bookmark")
                                    (agenda    . "nf-oct-calendar")
                                    (projects  . "nf-oct-briefcase")
                                    (registers . "nf-oct-database")))
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  )


(use-package cnfonts
  :ensure t
  :config
  (cnfonts-mode 1)
  )


(use-package org-modern              
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
   ;; 启用美化表格
  (setq org-modern-table t)
  
  ;; 启用美化标题
  (setq org-modern-headline t)
  
  ;; 启用漂亮的时间戳格式
  (setq org-modern-time-stamp t)
  
  ;; 启用任务优先图标
  (setq org-modern-priority t)

  ;; 启用任务状态图标
  (setq org-modern-task-status t)
  
  (global-set-key (kbd "C-c o m") 'org-modern-mode)
  )                                 


;; 使用 visual-fill-column 让文本居中并限制文本宽度，提升可读性
(use-package visual-fill-column
  :ensure t
  :config
  (setq-default visual-fill-column-center-text t)      ;; 启用文本居中
  (setq-default visual-fill-column-width 100)        ;;设置文本宽度为80字符
  :hook (
         (org-mode . visual-fill-column-mode)
         (prog-mode . visual-fill-column-mode)
         )            ;; 在 org-mode 中启用
  )

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))


(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  :config
  ;; Whether display the icons
  (setq nerd-icons-ivy-rich-icon t)

  ;; Whether display the colorful icons.
  ;; It respects `nerd-icons-color-icons'.
  (setq nerd-icons-ivy-rich-color-icon t)

  ;; The icon size
  (setq nerd-icons-ivy-rich-icon-size 1.0)
  )


(use-package nerd-icons-corfu
  :ensure t)


;; Initial frame
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.628)
                            (height . 0.8)
                            (fullscreen)))
(use-package posframe
  :ensure t)


(use-package ivy-posframe
  :ensure t
  :after ivy
  :config
  ;; 基本显示设置
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          (width . 50)
          (height . 10)
          (internal-border-width . 2)
          (internal-border-color . "#888")
          (background-color . "#f0f0f0")
          (foreground-color . "#000000")))

  ;; 设置弹窗自动消失的时间
  (setq ivy-posframe-timeout 1)

  ;; 启用 ivy-posframe
  (ivy-posframe-mode 1))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-time t)   ;; 启用时间显示
  (display-time-mode t)         ;; 启用显示时间模式
  (doom-modeline-mode 1))


(provide 'init-themes)
;;; init-themes.el ends here
