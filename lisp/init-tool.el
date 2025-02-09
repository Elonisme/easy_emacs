;;; init-basic --- 基础的配置
;;; Commentary:

;;; code:

;; 矩阵编辑
(use-package multiple-cursors
  :ensure t
  )

;; 安装 all-the-icons
(use-package all-the-icons
  :ensure t)

;; 设置侧边目录
(use-package neotree
  :ensure t
  :bind
  (("C-c r" . neotree-refresh)   ;; 绑定 Ctrl+c r 来刷新 neotree
   ([f8] . neotree-toggle))      ;; 绑定 F8 键来打开/关闭 neotree
  :config
  (setq neo-smart-open t)        ;; 自动打开当前文件所在的目录
  (setq neo-theme 'icons)        ;; 使用图标主题（需要安装 'all-the-icons'）
  )

;; 快捷键提示
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))


(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)                ;; 设置模式行主题
  (setq sml/no-confirm-load-theme t)          ;; 加载主题时不需要确认
  (sml/setup)
  )                              

;; 安装和配置 Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper) ;; 替代默认的搜索
         ("C-r" . swiper) ;; 反向搜索
         ("C-c C-r" . swiper-thing-at-point)) ;; 搜索当前位置的内容
  :diminish
  :config
  (setq swiper-action-recenter t)) ;; 确保匹配项居中显示

(use-package company
  :ensure t
  :hook ((org-mode . company-mode)
         (emacs-lisp-mode . company-mode))
  :config
  (setq company-idle-delay 0.1        ;; 自动补全延迟时间
        company-minimum-prefix-length 1 ;; 补全触发的最小前缀长度
        company-selection-wrap-around t) ;; 启用循环选择
  )

;; 启用 yasnippet 的全局模式
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; 重启 emacs
(use-package restart-emacs
  :ensure t)

(provide 'init-tool)
;;; init-basic.el ends here

