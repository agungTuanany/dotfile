;;; init.el --- emacs  -*- lexical-binding: t; outline-regexp: ";;;"; eval: (local-set-key (kbd "C-c i") #'consult-outline) -*-
;;
;; Copyright (c) 2022-2023 <agung.tuanany@gmaildot.com>
;; Author: Agung Tuanany <info.tuanany.eu.org>
;;
;; Version: 0.1.0
;; Package-Requires:
;;
;; Commentary:
;; Thanks to Protosilaos-Stavrou and System-Crafter aka David Wilson for
;; Inspiring me to write down my own emacs-configuration from base level
;; (scratch) and grown up.
;;
;; Code:

;;;; Basic configuration of  built-in feature
(electric-pair-mode 1)                                   ; Auto-closing brackets
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode 1)

(setq initial-buffer-choice t)                           ; Always start with *scratch*
(setq frame-title-format '("%b (%f)"))
(setq ring-bell-function 'ignore)
(setq use-short-answer t)
(setq native-comp-async-report-warnings-errors 'silent)  ; Emacs 28 with native compilation
(setq native-compile-prune-cache t)                      ; Emacs 29

(setq auto-save-default nil)      
(setq make-backup-files nil)
(setq backup-inhibited nil)                              ; Not sure if needed. given 'make-backup-file'
(setq create-lockfiles nil)                              ; No need for ~ files when editing
(setq blink-cursor-mode nil)                             ; Disable blinking cursor

(setq inhibit-compacting-font-caches t)
(setq visible-bell t)
(setq echo-keystrokes 0.05)
(setq file-name-handler-alist nil)
(setq load-prefer-newer t)                               ; load newest version of a file
(setq frame-inhibit-implied-resize t)

(setq-default fill-column 80)
(setq-default display-line-numbers 'relative)
;; disable line-numbers for some modes
(dolist (mode '(Custom-mode-hook
                Info-mode-hook
                compilation-mode-hook
                dired-mode-hook
                eshell-mode-hook
                help-mode-hook
                magit-mode-hook
                markdown-mode-hook
                org-mode-hook
                package-menu-mode-hook
                shell-mode-hook
                term-mode-hook
                text-mode-hook
                treemacs-mode-hook
		dashboard-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))




(setq dired-listing-switches "-alh")                     ; Make dired human-readable size
(setq indent-tabs-mode nil)
(setq-default tab-width 4)
(setq show-paren-style 'parenthesis
      show-paren-delay 0)
;;(show-paren-mode)

;; TODO: setup tabs for indentation

;; **NOTE: THIS SHOULD BE THE FIRST THING TO EVAL AFTER BASIC SETTINGS**
;;(set-cursor-color "yellow")
;;(load-theme 'tango-dark)
(auto-revert-mode 1)                                     ; Refresh the file if has changed
(set-fringe-mode 10)                                     ; Give some breathing room


;;; Key Mapping
(keymap-global-unset "C-x C-c" 'save-buffers-kill-terminal)
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-x C-c C-c" 'save-buffers-kill-emacs)
(keymap-global-set "C-x M-f" 'recentf-open-files)

;; set frame parameters
(add-to-list 'default-frame-alist '(fullscreen, maximized))
(add-to-list 'default-frame-alist '(alpha-background . 90)) ; Give transparency

;;;; PACKAGES
;; Presetup package sources
(require 'package)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
	("melpa" . 2)
	("org" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-install 'use-package)1)

(require 'use-package)
(setq use-package-always-ensure t)

(dolist (emacs-module-path '("tuanany-lisp" "tuanany-emacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file emacs-module-path)))

;; LOAD EMACS MODULAR CONFIG FILES
(defvar addons
  '("tuanany-ui.el"
    "tuanany-mode-line.el"
    "tuanany-emacs-dashboard.el"
    ;;; COMPLETIONS
    "tuanany-custom-completion.el"
    "tuanany-vertico-completion.el"
    "tuanany-company-completion.el"
    "tuanany-magit.el"
    "tuanany-evil.el"
    ;; tuanany-lisp
    "tuanany-orderless.el"
    "tuanany-spell-checker.el"
    ;;; PROGRAMMING LANGUAGES SPECIFIED
    "tuanany-setup-clojure.el"
    "tuanany-setup-racket.el"
    "tuanany-setup-html.el"
    ;;"tuanay-setup-js.el"
    ))

(dolist (x addons)
  (load x))

;;========================================
;; Manage Backup, autosave, custom, backup

;; Put all the files system on '~/.config/emacs/etc' directory
;; Do not clutter base emacs directory <~/.config/emacs>, all unnecessary or secondary file
(setq custom-file (locate-user-emacs-file "etc/custom.el"))
(load custom-file)

;; remember and restore the last place you visited in a file
(setq save-place-file (locate-user-emacs-file "etc/saveplaces"))
(save-place-mode 1)

(setq auto-save-list-file-prefix "~/.config/emacs/etc/auto-save-list/.save-")
(setq recentf-save-file "~/.config/emacs/etc/recentf")
(recentf-mode 1)

;; Enable these
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))

;; And disable these
(dolist (c '(eshell project-eshell overwrite-mode iconify-frame diary))
  (put c 'disabled t))
(put 'eshell 'disabled nil)

;;
(setq kill-do-not-save-duplicates t)
(setq eval-expression-print-length nil)
;;(setq duplicate-line-final-position -1)
;;(setq duplicate-region-final-position -1)
