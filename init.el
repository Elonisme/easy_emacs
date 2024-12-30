;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Initialize the package management system early
(setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/packages/")
                         ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)


;; loading lisp directory
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; loading other packages
;; not showing meassgae in`*message*' buffer
(with-temp-message ""
  (require 'init-base)
  (require 'init-basic)
  (require 'init-write)
  (require 'init-prog)
  (require 'init-themes)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("45631691477ddee3df12013e718689dafa607771e7fd37ebc6c6eb9529a8ede5" "0a2168af143fb09b67e4ea2a7cef857e8a7dad0ba3726b500c6a579775129635" "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875" "6819104c5f7d70485b32c10323aa396806d282fcee5b707e462bf3d156f44c39" "48d34b6afe72407ca494387c8bea495bb2deee96bd88516f302db1f11e1810a1" default))
 '(package-selected-packages
   '(dashboard smart-mode-line ace-window amx catppuccin-theme yasnippet-snippets which-key visual-fill-column restart-emacs quickrun org-superstar org-fragtog org-download org-auto-tangle org-appear neotree multiple-cursors markdown-preview-mode counsel company-statistics company-box color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
