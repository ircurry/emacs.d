;; annoying GUI stuff
(setq inhibit-startup-message t)    ;; Starts on blank screen
(scroll-bar-mode -1)                ;; Disable visible scrollbar
(tool-bar-mode -1)                  ;; Disable tool bar
(menu-bar-mode -1)                  ;; Disable menu bar
(tooltip-mode -1)                   ;; Disable tooltips
(set-fringe-mode 10)                ;; No idea what this does

(setq warning-minimum-level :error)

;; Font
(add-to-list 'default-frame-alist
             '(font . "mono-11"))

;; Setting Opacity
(set-frame-parameter (selected-frame) 'alpha '(100 . 50))
 (add-to-list 'default-frame-alist '(alpha . (100 . 50)))
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
 (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; Packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Setting Lines
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
	        term-mode-hook
		shell-mode-hook
	        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Doom modeline                             ;note that you need to run M-x all-the-icons-install-fonts when first installed
(use-package all-the-icons)
  
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (display-battery-mode 1)
  :custom (doom-modline-height 10))

;; Rainbow Delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 3))

;; Ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Councel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Themes
(use-package doom-themes)
(use-package cyberpunk-theme)
(use-package catppuccin-theme)
(use-package xresources-theme)
(load-theme 'xresources t)             ; Xresource theme
;; (load-theme 'doom-nord t)              ; Winter
;; (load-theme 'catppuccin t)             ; Spring
;; (load-theme 'doom-solarized-dark t)    ; Summer Nights
;; (load-theme 'doom-solarized-light t)   ; Summer Days
;; (load-theme 'doom-gruvbox t)           ; Fall/Autumn
;; (load-theme 'doom-lantern t)           ; Ruralish theme
;; (load-theme 'doom-henna t)             ; Sea themed
;; (load-theme 'doom-sourcerer t)         ; Foresty theme
;; (load-theme 'doom-laserwave t)         ; Awesome Fucking lasers
;; (load-theme 'doom-peacock t)           ; Goes good with fall train picture

;; Better Keybindings
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer cur/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(cur/leader-keys
  "t"   '(:ignore t :which-key "toggles")
  "tt"  '(counsel-load-theme :which-key "choose theme")
  "w"   '(:ignore t :which-key "windows")
  "wb"  '(kill-some-buffers :which-key "kill multiple buffers")
  "RET" '(vterm :which-key "vterm-other-window"))

;;(general-define-key
;; "C-M-j" 'counsel-switch-buffer)

;; Magit
(use-package magit
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; Becoming Evil
(defun cur/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
   (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init 
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . cur/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) 
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(evil-mode 1)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(cur/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/proj/code")
    (setq projectile-project-search-path '("~/proj/code" "~/proj/case")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
 :after projectile
 :config
 (counsel-projectile-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  
  (setq org-agenda-files
	'("~/org/tasks.org"))
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; Setting up terminal modes
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;;(setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

;; LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package rust-mode)
(setq '(sh-basic-offset 8))
(setq '(sh-indentation 8))
(setq-default c-basic-offset 8)

(use-package yuck-mode)


(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "0d2882cc7dbb37de573f14fdf53472bcfb4ec76e3d2f20c9a93a7b2fe1677bf5" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(package-selected-packages
   '(rust-mode lsp-mode vterm ewal auto-package-update evil-magit magit counsel-projectile projectile evil-collection evil general all-the-icons catppuccin-theme cyberpunk-theme doom-themes helpful counsel ivy-rich rainbow-delimiters rainbow-delimeters doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
