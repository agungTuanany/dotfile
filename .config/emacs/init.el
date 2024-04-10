;;;; init.el --- emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: init file, key mapping, modular file, custom config.

;;;; Package-Requires:

;;;; License:


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Recommended to have this at the top
;; Source: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; Emacs 29 support for tree-sitter built in.
(setq treesit-extra-load-path `(,(concat user-emacs-directory "/etc/var/tree-sitter-dist/")
                                ,(concat user-emacs-directory "/etc/tree-sitter")))

(use-package no-littering :ensure t)
(use-package quelpa :ensure t)
(use-package quelpa-use-package :ensure t)

(use-package emacs
  :ensure nil
  :defer
  :custom
  (initial-buffer-choice t)                     ; open *scratch* buffer
  (electric-pair-mode t)                        ; Automatic parens pairing
  (toggle-frame-maximized)                      ; Always use all screen for emacs
  (pending-delete-mode)                         ; Typed text replaces if the selection is active
  (use-short-answers t)                         ; use 'y' or 'n'
  (indent-tabs-mode nil)                        ; tabs are evil
  (narrow-to-defun-include-comments t)
  (confirm-nonexistent-file-or-buffer nil)

  (switch-to-buffer-obey-display-actions t)     ; Treat manual switching of buffers the same as programmatic
  (switch-to-buffer-in-dedicated-window nil)
  (window-sides-slots '(3 0 3 1))               ; 
  (sentence-end-double-space nil)               ; End sentence with 1 space not 2
  (x-stretch-cursor t)                          ; Make cursor stretch to cover wider characters
  (minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (history-delete-duplicates t)
  (auto-revert-mode t)                          ; Refresh the file if has changed
  (set-fringe-mode 6)                           ; Give some breating room for symbols

  (prefer-coding-system 'utf-8)
  (define-coding-system-alias 'UTF-8 'utf-8)
  (debugger-stack-frame-as-list t)
  (use-dialog-box nil)
  (scroll-conservatively 100)


  (inhibit-compacting-font-caches t)            ; Do not exhaust GC memory
  (visible-bell t)
  (blink-cursor-mode nil)                       ; Blink gives me a sick eyes
  ;; (file-name-handler-alist nil)
  ;; (echo-keystrokes 0.05) ; the value is 0.25
  (load-prefer-newer t)                         ; Load newest version file
  (frame-inhibit-implied-resize nil)            ; Fonts independent from any resized frame
  (tab-width 4)
  (show-paren-style 'expression)
  (show-paren-delay 0)
  (fill-colum 120)
  (dired-listing-switches "-alh")
  (kill-do-not-save-duplicates t)
  (eval-expression-print-length nil)
  (display-line-numbers 'relative)

  ;; FRAMES.
  ;; The top-level window in the GUI version of emacs
  (add-to-list 'default-frame-alist '(alpha-background . 90)) ; Give transparency

  ;; do not clutter root folder
  (auto-save-default nil)
  (make-backup-files nil)
  (backup-inhibited nil)
  (create-lockfiles nil)
  :config
  ;;========================================
  ;;; CORE
  ;;========================================
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)

  (dolist (mode '(Custom-mode-hook
                  Info-mode-hook
                  lisp-interaction-mode-hook
                  ibuffer-mode-hook
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
                  helpful-mode-hook
                  messages-buffer-mode-hook
                  dashboard-mode-hook))
    (add-hook mode(lambda () (display-line-numbers-mode -1))))

  ;;========================================
  ;;; KEY MAPPING
  ;;========================================

  (keymap-global-unset "C-x C-c" 'save-buffers-kill-terminal)
  (keymap-global-unset "C-x C-o" 'delete-blank-lines)
  (keymap-global-set "C-x C-b" 'ibuffer)
  (keymap-global-set "C-x C-c C-c" 'save-buffers-kill-emacs)
  (keymap-global-set "C-x M-f" 'recentf-open-files)
  (keymap-global-set "C-x C-o" 'other-window)
  (keymap-global-set "C-c f" 'find-name-dired)
  (keymap-global-set "C-c s" 'find-lisp-find-dired)


  (dolist (tuanany-module-path '("tuanany-emacs-modules" "tuanany-lisp"))
    (add-to-list 'load-path
                 (locate-user-emacs-file tuanany-module-path)))

  (defvar addons '(
                   ;;; UI
                   "tuanany-ui-dashboard.el"
                   "tuanany-ui-modeline.el"
                   "tuanany-ui-time.el"
                   "tuanany-ui.el"
                   ;;; COMPLETION
                   "tuanany-completion-custom.el"
                   "tuanany-completion-vertico.el"
                   "tuanany-completion-corfu.el"
                   "tuanany-completion-cape.el"
                   "tuanany-completion-orderless.el"
                   "tuanany-completion-consult.el"
                   "tuanany-completion-marginalia.el"
                   "tuanany-completion-embark.el"
                   ;;; HELPER
                   "tuanany-helper-dumb-jump.el"
                   "tuanany-helper-helpful.el"
                   "tuanany-helper-ispell.el"
                   "tuanany-helper-minibuffer.el"
                   ;; "tuanany-helper-shrface.el"
                   ;;; TOOLS
                   "tuanany-tools-evil.el"
                   "tuanany-tools-magit.el"
                   "tuanany-tools-proced.el"
                   "tuanany-tools-projectile.el"
                   "tuanany-tools-shell.el"
                   "tuanany-tools-eww.el"
                   "tuanany-tools-tree-sitter.el"
                   ;; "tuanany-tools-browse-url.el"
                   ;; "tuanany-tools-prog-mode.el"
                   ;;; LANGUAGES
                   "tuanany-lang-clojure.el"
                   "tuanany-lang-racket.el"
                   "tuanany-lang-web-mode.el"
                   "tuanany-lang-js2-mode.el"
                   ))

  (dolist (x addons)
    (load x))

  ;;========================================
  ;;; FILE-HANDLING
  ;; Manage Backup, autosave, custom, backup
  ;;========================================

  ;; Put all the files system on '~/.config/emacs/etc' directory
  ;; Do not clutter base emacs directory <~/.config/emacs>,
  ;; all unnecessary or secondary file
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

  ) ;; END EMACS-PACKAGE
