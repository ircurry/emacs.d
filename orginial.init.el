;;Disabling the anoying GUI stuff
(tool-bar-mode -1)
(menu-bar-mode -1)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;;Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))
;; Language Server Setup
;;(use-package lsp-mode
;;  :ensure t
;;  :config
;;  (add-hook 'rust-mode-hook #'lsp)) 
;; 


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(which-key use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you endit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
