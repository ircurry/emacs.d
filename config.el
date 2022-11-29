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

(setq inhibit-startup-message t)    ;; Starts on blank screen
(scroll-bar-mode -1)                ;; Disable visible scrollbar
(tool-bar-mode -1)                  ;; Disable tool bar
(menu-bar-mode -1)                  ;; Disable menu bar
(tooltip-mode -1)                   ;; Disable tooltips
(set-fringe-mode 10)                ;; No idea what this does

;; (add-to-list 'default-frame-alist
;;              '(font . "mono-11"))
(add-to-list 'default-frame-alist
             '(font . "Terminus-12"))

(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
  (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;;Uncomment for no transparency
 (set-frame-parameter (selected-frame) 'alpha '(100 . 50))
  (add-to-list 'default-frame-alist '(alpha . (100 . 50)))

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; note that you need to run M-x all-the-icons-install-fonts when first installed
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon t)
  (display-battery-mode 1)
  :custom (doom-modline-height 10))

(use-package autothemer
  :ensure t)
(use-package doom-themes)
(use-package cyberpunk-theme)
(use-package catppuccin-theme)
;; (load-theme 'doom-laserwave t)         ; Awesome Fucking lasers
;; (load-theme 'doom-gruvbox t)           ; Gruvbox
;; (load-theme 'everblush t)
;; (load-theme 'catppuccin-mocha t)
;; (load-theme 'kanagawa t)
(load-theme 'ewal-doom-one t)

;; (use-package xresources-theme)
;; (load-theme 'xresources t)

(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))
  ;; :if (not window-system)
  ;; :config
  ;; (setq-default mode-line-format nil))

(use-package ewal-doom-themes
    :if window-system
    :init (progn
            (show-paren-mode +1)
            (global-hl-line-mode)))

(defun cur/reload-ewal-theme ()
  (interactive)
  (load-theme 'ewal-doom-one t)
  (set-face-attribute 'org-block nil :foreground nil :background (ewal-load-color 'background +1) :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :foreground nil :background (ewal-load-color 'background +1) :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-end-line nil :foreground nil :background (ewal-load-color 'background +1) :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun cur/reset-theme ()
  (interactive)
  (set-face-attribute 'org-block nil :foreground nil :background nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :foreground nil :background nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-end-line nil :foreground nil :background nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun cur/set-theme ()
  (interactive)
  (cur/reset-theme)
  (counsel-load-theme)
  (cur/org-font-setup))

(use-package magit
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package general
  :config
  (general-evil-setup t)

(general-create-definer cur/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(cur/leader-keys
  "s"   '(swiper :which-key "toggles")
  ;; Togling
  "t"   '(:ignore t :which-key "toggles")
  "tt"  '(cur/set-theme :which-key "choose theme")
  ;; Buffers
  "b"   '(:ignore t :which-key "buffers")
  "bs"  '(kill-some-buffers :which-key "kill multiple buffers")
  "bc"  '(kill-current-buffer :which-key "kill current buffer")
  "bC"  '(kill-buffer :which-key "kill a buffer")
  "bb"  '(counsel-ibuffer :which-key "switch buffer")
  "bn"  '(next-buffer :which-key "next buffer")
  "bp"  '(previous-buffer :which-key "previous buffer")
  "bl"  '(ibuffer :which-key "list buffers")
  "r"   '(:ignore t :which-key "reload")
  "rt"  '(cur/reload-ewal-theme :which-key "reload")
  ;; Other stuff
  "g"   '(magit-status :which-key "magit")
  "f"   '(counsel-find-file :which-key "find or make file")
  "RET" '(vterm :which-key "vterm"))



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

;; Won't enable properly in :config :(
(evil-mode 1)

(use-package evil-collection
  :after evil
  :config
  ;; (setq evil-collection-mode-list '(dashboard ibuffer))
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(cur/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

(general-define-key
 "M-h" 'evil-window-left
 "M-j" 'evil-window-down
 "M-k" 'evil-window-up
 "M-l" 'evil-window-right
 "M-n" 'split-and-follow-horizontally
 "M-m" 'split-and-follow-vertically
 )

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defhydra cur/window-management (:hint nil)
  ("c"  (delete-window) "delete window" :exit t)
  ("h"  evil-window-left)
  ("j"  evil-window-down)
  ("k"  evil-window-up)
  ("l"  evil-window-right)
  ("o"  evil-window-next)
  ("n"  split-and-follow-horizontally)
  ("m"  split-and-follow-vertically)
  ("RET" nil :exit t))
(cur/leader-keys
  "w" '(cur/window-management/body :which-key "window management"))

(defun opacity-none ()
    (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 . 50))
  (add-to-list 'default-frame-alist '(alpha . (100 . 50))))

(defun opacity-some ()
    (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
  (add-to-list 'default-frame-alist '(alpha . (85 . 50))))

(cur/leader-keys
  "to" '(:ignore t :which-key "opacity")
  "too" '(opacity-some :which-key "transparent background")
  "ton" '(opacity-none :which-key "hard background"))

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
(cur/leader-keys
  "p"  '(:ignore t :which-key "projectile")
  "pp" '(projectile-dired :which-key "open project in dired")
  "pf" '(projectile-find-file :which-key "open a project's file")
  "ps" '(projectile-switch-project :which-key "switch project"))

(use-package counsel-projectile
 :after projectile
 :config
 (counsel-projectile-mode 1))

(defun cur/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun cur/org-font-setup ()
  ;; Replace list hyphen with dot
  (interactive)
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

  ;; Ensure that anthing that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil :foreground nil :background (ewal-load-color 'background +1) :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block-begin-line nil :foreground nil :background (ewal-load-color 'background +1) :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block-end-line nil :foreground nil :background (ewal-load-color 'background +1) :inherit 'fixed-pitch)

  ;;; Reset Background ----------------------------------------
  ;; (set-face-attribute 'org-block nil :foreground nil :background nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block-begin-line nil :foreground nil :background nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-block-end-line nil :foreground nil :background nil :inherit 'fixed-pitch)
  ;; ----------------------------------------------------------

  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . cur/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/org/tasks.org"
          "~/org/completed.org"))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (cur/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cur/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cur/org-mode-visual-fill))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("tex" . "src latex"))

(general-define-key
 :key-maps 'org-mode
 "C-S-J" 'outline-move-subtree-down
 "C-S-K" 'outline-move-subtree-up
 "C-S-H" 'outline-promote
 "C-S-L" 'outline-demote
 )
(define-key org-mode-map (kbd "<normal-state> M-h") nil)
(define-key org-mode-map (kbd "<normal-state> M-j") nil)
(define-key org-mode-map (kbd "<normal-state> M-k") nil)
(define-key org-mode-map (kbd "<normal-state> M-l") nil)
(define-key org-mode-map (kbd "M-l") nil)
(define-key org-mode-map (kbd "M-h") nil)

(defun cur/fix-org-fonts ()
  (interactive)
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;;(setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(setq sh-basic-offset 8)
(setq sh-indentation 8)
(setq-default c-basic-offset 8)

(use-package beacon)
(beacon-mode 1)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner "~/.emacs.d/avatar.png")
  (setq dashboard-banner-logo-title "")
  (setq dashboard-items '((projects . 6)
                          (bookmarks . 6)
                          (recents  . 8)))
  (dashboard-setup-startup-hook)
  (setq dashboard-footer-messages '("\"I discovered freedom for the first time in England.\" - Emperor Hirohito"))
  ;; (setq dashboard-footer-icon (all-the-icons-octicon "ghost"
  ;;                                              :height 1.1
  ;;                                              :v-adjust -0.05
  ;;                                              :face 'font-lock-keyword-face))
  )
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package web-mode
  :ensure t
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t)
  :mode
  (".html$" "*.php$" "*.tsx"))

(use-package emmet-mode
  :ensure t)

(use-package rust-mode)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))
