;;; init-prog --- 编程模块
;;; commentary:
;;; Code:

;; 运行的代码文件
(use-package quickrun
  :ensure t
  :bind
  ("C-c C-r" . quick-run)  ;; 绑定快捷键以快速运行代码
  :config
  (setq quick-run-focus-p nil) ;; 运行时不切换到结果窗口
  )

(provide 'init-prog)
;;; init-prog.el ends here

