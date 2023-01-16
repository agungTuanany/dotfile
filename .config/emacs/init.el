;;=======================================
;;; Presetup Begin
(setq inhibit-startup-message t
      visible-bell t)

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable toolbar
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(electric-pair-mode 1)     ; auto-closing brackets
(auto-revert-mode 1)       ; refresh the file if has changedee

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

(load-theme 'tango-dark t)

;;========================================
;; Make <ESC> quit prompt either to <C-g>
(global-set-key (kbd "<escapge>") 'keyboard-escape-quit)

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq auto-save-file-name-transforms
      '((".*" "~/.config/emacs/auto-save-list/" t)))

(setq backup-directory-alist
      '((".*" . "~/.config/emacs/backups")))

;;; Presetup End
;;========================================
