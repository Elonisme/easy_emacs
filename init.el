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
;; end init.el
