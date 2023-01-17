;;; Package -- init.el
;;
;;; Commentary:
;; use systemlinks from the repo:
;; ln -s ~/Repo/agung_dotfile/.config/emacs ~/config/

;;=======================================
;;; Presetup Begin
(setq inhibit-startup-message t
      visible-bell t)

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable toolbar
(tooltip-mode -1)          ; Disable tootips
(menu-bar-mode -1)         ; Disbale menu on top
(set-fringe-mode 10)       ; Give some breathing rome
(electric-pair-mode 1)     ; auto-closing brackets
(auto-revert-mode 1)       ; refresh the file if has changedee

(add-to-list 'default-frame-alist '(fullscreen, maximzed))

;; line-number config
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

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

(setq-default fill-colum 80)

;; theme config
(load-theme 'tango-dark t)

;;(set-face-attribute 'default nil :font "Liberation Mono" :height 90)
(set-face-attribute 'default nil :font "Source Code Pro" :height 90)

;; keybinding config
;; Make <ESC> quit prompt either to <C-g>
(global-set-key (kbd "<escapge>") 'keyboard-escape-quit)

;; remember recent opened files
(recentf-mode 1)

;; remembering minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; remember and restore the last place you visited in a file
(save-place-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;========================================
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

;;=======================================
;;; Magit Begin

;;; Magit End
;;=======================================

;; Ivy completion

;; company completion - for code/text completion

;;=======================================
;;; Lsp Begin

;;; Lsp End
;;=======================================

;;=======================================
;;; Magit Begin

;;; Magit End
;;=======================================
