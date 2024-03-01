;;;; init.el --- emacs  -*- lexical-binding: t; outline-regexp: ";;;"; eval: (local-set-key (kbd "C-c i") #'consult-outline) -*-
;;
;;;; Copyright (c) 2022-2023 <agung.tuanany@gmaildot.com>
;; Author: Agung Tuanany <info.tuanany.eu.org>
;;
;;;; Version: 0.1.0
;; Package-Requires:
;;
;;;; Commentary:
;; Thanks to Protosilaos-Stavrou and System-Crafter aka David Wilson for
;; Inspiring me to write down my own emacs-configuration from base level
;; (scratch) and grown up.
;; - Update:
;; 01/03/24
;; https://github.com/gopar/.emacs.d
;; @gopar have repo as such hidden gem, without no ambiguities utilize basic
;; craft resources.
;;
;;;; Code:

;; Recommended to have this at the top
;; Source: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; Emacs 29 support for tree-sitter built in.
(setq treesit-extra-load-path `(,(concat user-emacs-directory "/etc/var/tree-sitter-dist/")
                                ,(concat user-emacs-directory "/etc/tree-sitter")))
(setq load-prefer-newer t)
(use-package no-littering :ensure t)
(use-package quelpa :ensure t)
(use-package quelpa-use-package :ensure t)

(use-package emacs
  :ensure nil
  :defer

  :hook ((after-init . pending-delete-mode)
         (after-init . toggle-frame-maximized)
         (after-init . (lambda () (menu-bar-mode -1)))
         (after-init . (lambda () (scroll-bar-mode -1)))
         (after-init . (lambda () (tool-bar-mode -1)))
         (after-init . (lambda () (tooltip-mode -1)))
         )

  :custom
  (electric-pair-mode 1)    ; Auto-closing brackets
  (debugger-stack-frame-as-list t)
  (narrow-to-defun-include-comments t)
  (use-short-answer t)
  (confirm-nonexistent-file-or-buffer nill)
  ;; Treat manual switching of buffers the same as programatic
  (switch-to-buffer-obey-display-actions t)
  (switch-to-buffer-in-dedicated-window nil)
  (window-sides-slots '(3 0 3 1))
  ;; Sentences end with 1 space not 2
  (sentence-end-double-space nil)
  ;; Make cursor the width of the character it is under
  ;; ie. full width of a TAB
  (x-strecth-cursor t)
  ;; Stop cursor from going into minibuffer prompt text
  (minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (history-delete-duplicates t)
  ;; Completion stuff for consult
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (use-dialog-box nil)          ; Lets consistent and use minibuffer for everything
  (scroll-conservatively 100)
  (auto-revert-mode 1)                                    ; Refresh the file if has changed
  (set-fringe-mode 10)                                    ; Give some breathing room

  :config
  (prefer-coding-system 'utf-8)                           ; Uppercase is same as lowercase
  (define-coding-system-alias 'UTF-8 'utf-8)
  ;;(debugger-stack-frame-as-list t)                      ; display call-stack frame

  (setq initial-buffer-choice t)                          ; Always start with *scratch*
  (setq frame-title-format '("%b (%f)"))
  (setq ring-bell-function 'ignore)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)                     ; Emacs 29

  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq backup-inhibited nil)                             ; Not sure if needed. given 'make-backup-file'
  (setq create-lockfiles nil)                             ; No need for ~ files when editing
  (setq blink-cursor-mode nil)                            ; Disable blinking cursor

  (setq inhibit-compacting-font-caches t)
  (setq visible-bell t)
  (setq echo-keystrokes 0.05)
  (setq file-name-handler-alist nil)
  (setq load-prefer-newer t)                              ; load newest version of a file
  (setq frame-inhibit-implied-resize t)

  (setq-default fill-column 80)
  (setq-default display-line-numbers 'relative)
  (setq dired-listing-switches "-alh")                    ; Make dired human-readable size
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq show-paren-style 'parenthesis
        show-paren-delay 0)
  ;;(show-paren-mode)

  ;; set frame parameters
  (add-to-list 'default-frame-alist '(fullscreen, maximized))
  (add-to-list 'default-frame-alist '(alpha-background . 90)) ; Give transparency

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


  (setq kill-do-not-save-duplicates t)
  (setq eval-expression-print-length nil)
  ;;(setq duplicate-line-final-position -1)
  ;;(setq duplicate-region-final-position -1)

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

  (keymap-global-unset "C-x C-c" 'save-buffers-kill-terminal)
  (keymap-global-set "C-x C-b" 'ibuffer)
  (keymap-global-set "C-x C-c C-c" 'save-buffers-kill-emacs)
  (keymap-global-set "C-x M-f" 'recentf-open-files)

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
    ;;; LSP
      ;; "tuanany-setup-lsp.el"
    ;;; PROGRAMMING LANGUAGES SPECIFIED
      "tuanany-setup-clojure.el"
      "tuanany-setup-racket.el"
      "tuanany-setup-html.el"
      ;;"tuanay-setup-js.el"
      ))

  (dolist (x addons)
    (load x))
  );; --END--





;; **NOTE: THIS SHOULD BE THE FIRST THING TO EVAL AFTER BUILT-INS SETTINGS**
;;(set-cursor-color "yellow")
;;(load-theme 'tango-dark)

;;; Key Mapping




;;
