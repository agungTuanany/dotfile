(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; disable visible scroolbal
(tool-bar-mode -1)      ; disable the toolbar
(tooltip-mode -1)       ; disable tooltips
(menu-bar-mode -1)      ; disabnle menu bar
(set-fringe-mode 10)    ; give some breathing

;; set up the visible bell
(setq visible-bell t)

;; launch emacs as full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; set my fonts
;;(set-face-attribute 'default nil :font "Source Code Pro" :pixelsize=13)
(set-face-attribute 'default nil :font "Source Code Pro" :height 90)

;; set theme-colors
;;(load-theme 'tango-dark)
;;(load-theme 'wombat)

;; Make ESC quit prompts either to 'C-g'
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize
(require 'package)

;; enable line numbers as relative numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;;===============================================================
;; XXX RECOMMEND not using lambdas, instead create a function XXX
(defun do-not-display-line-number-mode-hook()
  "Hook to disable line-number-mode."
  (display-line-numbers-mode 0))

;; FIXME: 'add-hook' does not work!! to remove line-numbers
;;(add-hook 'org-mode-hook 'do-not-display-line-numbers-mode-hook)
;;(add-hook 'term-mode-hook 'do-not-display-line-numbers-mode-hook)
;;(add-hook 'eshel-mode-hook 'do-not-display-line-numbers-mode-hook)
;;(add-hook 'shell-mode-hook 'do-not-display-line-numbers-mode-hook)

;; OR
;;(dolist (mode '(org-mode-hook
;;		term-mode-hook
;;		shell-mode-hook
;;		eshell-mode-hook))
;;  (add-hook mode 'do-not-display-line-numbers-mode-hook))
;;===============================================================

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("Org" . "https://orgmode.org/elpa/")))
					;("elpa" . "https://elpa.gnu.org/
					;("melpa-stable" . "https://stable.melpa.org//pakages/")

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-packages on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; shows all used keybinding
;; to use this press: M-x 'clm/toggle-command-log-buffer'
(use-package command-log-mode)

;; enable global-command-log-mode
;; by hit: M-x global-command-log-mode

;; install 'diminish'
(use-package diminish
  :ensure t)

;; use Ivfy setup and Counsel for completion
;; NOTE: I'm not installing Counsel
(use-package ivy
  :diminish
  :bind (;;("C-s" . swiper)
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

;; Fri-Dec-17-2021
;; key binding in emacs
;; (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;; (define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 12)))

(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)  ;; move into (use-package general)
	 ("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-s" . counsel-grep-or-swiper)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))

  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; EXAMPLE:
;; "C-s" 'counsel-grep-or-swiper)
;;(general-define-key
;;"C-M-j" 'counsel-switch-buffer)

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; binding from insert mode to normal mode by "jj"
  ;;(define-key evil-insert-state-map (kbd "jj") 'evil-normal-state)
  ;;(define-key evil-insert-state-map (kbd "vv") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Project")
    (setq projectile-project-search-path '("~/Project")))
  (setq proejctile-switch-project-action #'projecttile-dired))

;; most often 'counsel-projectile' package use for searching with 'rgrep'
;; with this 'counsel-projectile-rg' function
;; and you can bind it into another self buffer with combine key (C-c s r) + (C-c C-o)
;; and you would also possible to press key 'M-o' in every rgrep result
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; 'evil-magit' package was removed from MELPA
;; instead use 'evil-collection' that had installed already

;; XXX Added by SYSTEM XXX
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-magit counsel-projectile which-key use-package rainbow-delimiters projectile ivy-rich hydra helpful general evil-collection doom-themes doom-modeline diminish-buffer diminish counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
