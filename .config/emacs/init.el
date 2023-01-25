;;; init.el --- emacs
;; Copyright (C) 2023
;; Author: ;;; Package -- init.el <agung.tuanany@gmaildotcom>
;; Keywords:
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
(when (not package-archive-contents)
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
      visible-bell t
      ;; adjust keystroke echo timeout
      echo-keystrokes 0.5)

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
;; fix line-number-mode when encountering an overly long line
;; from showing double question marks
(setq line-number-display-limit-width 10000)

;; Memory Management
(setq gc-cons-threshold 5000000)

;; disable legacy algorithms 3DES
(setq gnutls-min-prime-bits 2048)
(setq gnutls-algorithm-priority "SECURE128")

;; =*Scracth*=
(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)

;; initial buffer, let's display notes file instead as daily reminder
(setq remember-notes-bury-on-kill nil)
(setq remember-notes-initial-major-mode 'org-mode)
(setq initial-buffer-choice 'remember-notes)
;; There is a bit of mismatch between the keybindings 'remember-notes-mode' and 'org-mode'
(with-eval-after-load 'remember
  (define-key remember-notes-mode-map (kbd "C-c C-c") nil))

;; disable line-numbers for some mode
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                package-menu-mode-hook
                treemacs-mode-hook
                help-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

(custom-set-faces
 ;; disable background in line-number-current-line
 '(line-number-current-line ((t (:inherit default :foreground "#CFC0C5" :slant normal :weight bold))))
 ;; increase comment highlight, default one is lightly
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic))))
 )

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

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; set firefox as my default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-developer-edition")

;; shorten Yes/No prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable blinking cursor
(setq blink-cursor-mode nil)

;; TODO: enable hs-minor-mode for toggle or use somethin else

;; make recenter with "C-l" start from top
(setq recenter-positions '(top middle bottom))

;; 'ediff'. when using ediff prepare horizontal split
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; 'dired' prefer using one buffer unless another one is explicitly created
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; fix scrolling
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; stop pasting at the mouse click point
;; middle-clicking is nice to paste, however it should not adjust point and
;; paste at the end then adjusted point
(setq mouse-yank-at-point t)

;; Display fringe indicators and fix line movement in 'visual-line-mode~
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; I don't like the remappings done to operate on visual lines (for
;; =C-a=, =C-e= and =C-k=), so I'm just undefining them.
(setcdr visual-line-mode-map nil)

;; enable every deactive command
;; avoiding confusion for beginners, with many option to choose.
(setq disabled-command-function nil)

;; Save clipboard data of other programs in the kill ring when possible
(setq save-interprogram-paste-before-kill t)

;; don't print integers in multiple format
(fset 'eval-expression-print-format 'ignore)

;; print backtraces in a lispier form
(setq debugger-stack-frame-as-list t)

;; display raw bytes as hex
(setq display-raw-bytes-as-hex t)

;; disable signal handler insanity
(setq attempt-stack-overflow-recovery nil)
(setq attempt-orderly-shutdown-on-fatal-signal nil)

;; unconditionally kill subprocess at exit
(setq confirm-kill-processes nil)

;; sentence spacing
(setq sentence-end-double-space nil)

;;========================================
;; Manage Backup, autosave, custom, backup

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq auto-save-list-file-prefix "~/.config/emacs/autosave/")
(setq auto-save-file-name-transforms '((".*"  "~/.config/emacs/autosave/" t)))

;; Backup files are created on save in the same directory as the file and
;; end in =~=.  They can be numbered which makes most sense combined with
;; a different save location and automatic pruning.
(setq backup-directory-alist
      '((".*" . "~/.config/emacs/backups")))
(setq version-control t)
(setq delete-old-versions t)

;; Lock files are put in the same directory to detect multiple users
;; trying to edit the same file. Given that I'm on a single-user system,
;; I can migrate them elsewhere as of Emacs 28.1.
(setq lock-file-name-transforms '((".*" "~/.config/emacs/lock/" t)))

(setq bookmark-default-file "~/.config/emacs/etc/bookmarks")

;; remember and restore the last place you visited in a file
(save-place-mode 1)
(setq save-place-file "~/.config/emacs/etc/saveplace")

;; move recentf to etc for not cluttering base dir
(setq recentf-save-file "~/.config/emacs/etc/recentf"
      recentf-max-saved-items 50)

;; TRAMP https://www.gnu.org/software/tramp/
;; makes backup files, they should better be kept locally than remote
(setq tramp-backup-directory-alist backup-directory-alist)
(with-eval-after-load 'tramp-cache
  (setq tramp-persistency-file-name "~/.config/emacs/etc/tramp"))

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
;; config modeline. The bar information on bottom
(use-package doom-modeline
  :ensure t
  :custom(
          (doom-modeline-height 15))
  :init (doom-modeline-mode 1))

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

(use-package pdf-tools
  :pin manual
  :config
  ;;(pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
  )
;;=======================================
;;; Evil Begin
(use-package evil
  :ensure t
  :init
  ;; going for emacs state whenever it makes sense
  ;; respect and use emacs motion state (emacs movement)
  ;; (setq evil-default-state 'emacs
  ;;       evil-emacs-state-modes nil
  ;;       evil-insert-state-modes nil
  ;;       evil-motion-state-modes nil
  ;;       evil-normal-state-modes '(text-mode prog-mode fundamental-mode
  ;;                                           css-mode conf-mode TeX-mode
  ;;                                           diff-mode))
  ;; (add-hook 'org-capture-mode-hook 'evil-insert-state)
  ;; (add-hook 'with-editor-mode-hook 'evil-insert-state)
  ;; (add-hook 'view-mode-hook 'evil-emacs-state)

  ;; base setting adjustment
  ;; make movement, undo and search feel a bit less weird
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-undo-system 'undo-redo
        evil-cross-lines t
        evil-move-beyond-eol t
        evil-want-fine-undo t
        evil-symbol-word-search t)

  ;; enable 'C-w' in emacs-state
  (with-eval-after-load 'evil-vars
    (setq evil-want-C-w-in-emacs-state nil))

  ;; (with-eval-after-load 'evil-common
  ;;   (evil-declare-motion 'recenter-top-bottom))

  ;; 'Y' as yank to the end of the line
  (setq evil-want-Y-yank-to-eol t)

  ;; disable setting up anyting in 'insert state keymap'
  (setq evil-disable-insert-state-bindings t)

  :config
  ;; keybindings
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

  (evil-mode 1)
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

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

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

(use-package consult
  :ensure t
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  )
;;; COMPLETION CONFIG END
;;=======================================

;;=======================================
;;; Lsp Begin

;;; Lsp End
;;=======================================

;;=======================================
;;; Org-mode Begin
(defun my-org-font-setup ()
  "Replace list hyphen with dot"
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
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'semibold :height (cdr face) :slant: oblique)

    )

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil   :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  )

(defun my-org-mode-setup ()
  "Setup all org modes"
  (org-indent-mode)
  (variable-pitch-mode 1)
  )

(use-package org
  :hook (org-mode . my-org-mode-setup)
  :config
  (setq org-ellipsis " ▾" )

  (my-org-font-setup)
  )

;;; Org-mode End
;;=======================================

;; theme config
;(load-theme 'tango-dark t)

;;; TESTING CODE
;; Load custom theme
;; (add-to-list 'custom-theme-load-path "~/.config/emacs/theme/my-solarized.el")
;; (add-to-list 'load-path "~/.config/emacs/theme/my-solarized.el")
;; (defun my-load-theme (&optional frame)
;;   (with-selected-frame (or frame (selected-frame))
;;     (load-theme 'my-solarized t)))
;; (my-load-theme)
;; (add-hook 'after-make-frame-functions 'my-load-theme)

;; (add-to-list 'custom-theme-load-path "~/.config/emacs/theme")
;; (add-to-list 'load-path "~/.config/emacs/theme")
;; (defun my-load-theme (&optional frame)
;;   (with-selected-frame (or frame (selected-frame))
;;     (load-theme 'my-solarized t)))
;; (my-load-theme)
;; (add-hook 'after-make-frame-functions 'my-load-theme)
