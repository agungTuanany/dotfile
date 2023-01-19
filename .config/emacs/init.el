;;; Package -- init.el
;;
;;; Commentary:
;; use systemlinks from the repo:
;; ln -s ~/Repo/agung_dotfile/.config/emacs ~/config/

;;=======================================
;;; Presetup package sources Begin
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

;;; Presetup package sources End
;;=======================================

;;=======================================
;;; Presetup Begin
(setq inhibit-startup-message t
      visible-bell t)

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable toolbar
(tooltip-mode -1)          ; Disable tooltip
(menu-bar-mode -1)         ; Disable menu on top
(set-fringe-mode 10)       ; Give some breathing room
(electric-pair-mode 1)     ; auto-closing brackets
(auto-revert-mode 1)       ; refresh the file if has changed

(add-to-list 'default-frame-alist '(fullscreen, maximized))

;; line-number config
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; disable line-numbers for some mode
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                package-menu-mode-hook
                treemacs-mode-hook
                help-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;; disable background in line-number-current-line
(custom-set-faces
 '(line-number-current-line ((t (:inherit default :foreground "#CFC0C5" :slant normal :weight bold)))))

;; indentation config
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; trailing whitespace
(global-whitespace-mode)
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))

(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9])))

(setq-default fill-column 80)

;;(set-face-attribute 'default nil :font "Liberation Mono" :height 90)
(set-face-attribute 'default nil :font "Source Code Pro" :height 90)

;; keybinding config
;; Make <ESC> quit prompt either to <C-g>
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; remember recent opened files
(recentf-mode 1)

;; remember minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; remember and restore the last place you visited in a file
(save-place-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; set firefox as my default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-developer-edition")

;;========================================
;; Manage custom Elips support files
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq auto-save-file-name-transforms
     '((".*"  "~/.config/emacs/auto-save-list" t)))

(setq backup-directory-alist
     '((".*" . "~/.config/emacs/backups")))
;;; Presetup End
;;========================================



;;========================================
;;TODO:
;;; Plugins

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-theme-enable-bold t
        doom-theme-enable-italic t)
  (load-theme 'doom-monokai-classic t))

;; config modeline. The bar infromation on bottom
(use-package doom-modeline
  :ensure t
  :custom(
          (doom-modeline-height 15))
  :init (doom-modeline-mode 1) )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package treemacs
  :ensure t)

(use-package winum
  :ensure t
  :config
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (winum-mode))

;;=======================================
;;; Evil Begin
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "v") 'evil-visual-block)
  (define-key evil-normal-state-map (kbd "C-v") 'evil-visual-char)
  (define-key evil-visual-state-map (kbd "v") 'evil-visual-block)
  (define-key evil-visual-state-map (kbd "C-v") 'evil-visual-char)

  ;; use visual line motion even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
;;; Evil End
;;=======================================

;;=======================================
;;; COMPLETION CONFIG BEGIN
;;company completion - for code/text completion
(use-package company
  :ensure t
  ;; :after lsp-mode
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backends '(company-elisp))))
         (emacs-lisp-mode . company-mode))
  :init
  (company-mode 1)
  :config
  ;; double dashes implies internal function, Cause 'winum' package use 'M-' with a number.
  (company-keymap--unbind-quick-access company-active-map)
  (company-tng-configure-default)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

;; Vertico + Marginalia + Orderless
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :config
  (custom-set-faces
   '(marginalia-documentation ((t (:inherit nil :foreground "LemonChiffon4" :weight normal))))))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; (use-package consult
;;   :ensure t
;;   :init
;;   (setq register-preview-delay 0.5
;;         register-preview-function #'consult-register-format)
;;   (advice-add #'register-preview :override #'consult-register-window)
;;   )
;;; COMPLETION CONFIG END
;;=======================================

;;=======================================
;;; Lsp Begin

;;; Lsp End
;;=======================================

;;=======================================
;;; Org-mode Begin

;;; Org-mode End
;;=======================================

;; theme config
;;(load-theme 'tango-dark t)
