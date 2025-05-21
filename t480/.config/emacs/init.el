;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((Emacs "25.1"))
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

;; Add custom tree-sitter grammar path
(setq treesit-extra-load-path `(,(concat user-emacs-directory "etc/var/tree-sitter-dist/")
				,(concat user-emacs-directory "etc/tree-sitter")))

(use-package no-littering :ensure t)
(use-package quelpa :ensure t)
(use-package quelpa-use-package :ensure t)

(use-package emacs
  :ensure nil
  :defer nil

  ;; Variables that should be set before any packages
  :init
  (setq
   dired-listing-switches        "-alhv --group-directories-first"
   display-line-numbers-type     'relative
   fill-column                   80
   indent-tabs-mode              t
   scroll-conservatively         100
   sentence-end-double-space     nil       ;; End sentence with 1 space not 2
   tab-width                     4         ;; tabs are evil

   ;;; FILE-HANDLING & HISTORIC
   ;; Manage Backup, autosave, custom, backup.
   ;;
   ;; Put all the files system on '~/.config/emacs/etc' directory
   ;; Do not clutter base Emacs directory <~/.config/emacs>,
   ;; all unnecessary or secondary file
   ;; do not clutter root folder
   auto-save-default              nil
   make-backup-files              nil
   backup-inhibited               nil
   create-lockfiles               nil
   file-name-handler-alist        nil

   ;; remember and restore the last place you visited in a file
   auto-save-list-file-name (expand-file-name "etc/auto-save-list/.save-" user-emacs-directory)
   recentf-save-file        (locate-user-emacs-file "etc/recentf")
   save-place-file          (locate-user-emacs-file "etc/saveplaces")
   savehist-file            (locate-user-emacs-file "etc/savehist")
   custom-file              (locate-user-emacs-file "etc/custom.el")

   ;; Performance & misc
   inhibit-compacting-font-caches   t       ;; Do not exhaust GC memory
   frame-inhibit-implied-resize     t       ;; Fonts independent from any resized frame
   load-prefer-newer                t       ;; Load newest version file
   history-length                   1000
   history-delete-duplicates        t

   ;; Minibuffer & prompts
   confirm-nonexistent-file-or-buffer nil
   kill-do-not-save-duplicates        t
   use-short-answers                  t      ;; use 'y' or 'n'
   use-dialog-box                     nil
   initial-buffer-choice              t      ;; open *scratch* buffer
   minibuffer-prompt-properties       '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

   ;; Coding systems
   coding-system-aliases  (list (cons 'UTF-8 'utf-8))
   prefer-coding-system   'utf-8

   ;; UI frame default
   ;; note: adjust transparency as desired
   ;; The top-level window in the GUI version of emacs
   default-frame-alist '((alpha-background . 90))

   debugger-stack-frame-as-list           t
   eval-expression-print-length           nil
   narrow-to-defun-include-comments       t
   switch-to-buffer-in-dedicated-window   nil
   switch-to-buffer-obey-display-actions  t            ;; Treat manual switching of buffers the same as programmatic
   window-sides-slots                     '(3 0 3 1)
   x-stretch-cursor                       t            ;; Make cursor stretch to cover wider characters
   )

  :custom
  (display-line-numbers t)            ;; globally enable line numbers
  (echo-keystrokes      0.05)         ;; the value is 0.25
  (show-paren-delay     0)
  (show-paren-style     'expression)
  (visible-bell         t)

  ;; Load early packages and hooks
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . hs-minor-mode)
  (before-save-hook . tuanany--untabify-before-save)

  ;; After everything is loaded, configure modes and keybindings
  ;;
  :config
  ;; simple toggle of customizable variables
  (blink-cursor-mode       0)             ;; Blink gives me a sick eyes
  (electric-pair-mode      1)             ;; Automatic parens pairing
  (global-auto-revert-mode 1)             ;; Refresh the file if has changed
  (pending-delete-mode     1)             ;; Typed text replaces if the selection is active
  (recentf-mode            1)
  (save-place-mode         1)
  (savehist-mode           1)

  ;; UI tweaks
  (menu-bar-mode   -1)
  (menu-bar-mode   -1)
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)

  ;; Unset unwanted keys (e.g spurious touchscreen events)
  (global-unset-key [touchscreen-begin])
  (global-unset-key [touchscreen-end])
  (global-unset-key [touchscreen-update])

  ;; Keybindings
  (keymap-global-unset "C-x C-c" 'save-buffers-kill-terminal)
  (keymap-global-unset "C-x C-o" 'delete-blank-lines)
  (keymap-global-set "C-c f" 'find-name-dired)
  (keymap-global-set "C-x C-c C-c" 'save-buffers-kill-emacs)
  (keymap-global-set "C-x C-o" 'other-window)
  (keymap-global-set "C-x M-f" 'recentf-open-files)
  (keymap-global-set "C-x C-b" 'ibuffer)

  (toggle-frame-maximized)    ;; Always use all screen for emacs
  (set-fringe-mode 6)         ;; Give some breating room for symbols

  ;; Disable line-numbers in specific mode
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
		  ;; package-menu-hook
		  ;; u-mode-hook
		  shell-mode-hook
		  term-mode-hook
		  text-mode-hook
		  treemacs-mode-hook
		  helpful-mode-hook
		  messages-buffer-mode-hook
		  dashboard-mode-hook))
    (add-hook mode(lambda () (display-line-numbers-mode -1))))


  (dolist (tuanany-module-path '("tuanany-emacs-modules" "tuanany-lisp"))
    (add-to-list 'load-path
		 (locate-user-emacs-file tuanany-module-path)))

  (defvar tuanany-addons '(
			   ;; ==== UI ====
			   "tuanany-ui-dashboard.el"
			   "tuanany-ui-doom-modeline.el"
			   "tuanany-ui-time.el"
			   "tuanany-ui.el"
			   ;; ==== COMPLETION ====
			   "tuanany-completion-cape.el"
			   "tuanany-completion-consult.el"
			   "tuanany-completion-corfu.el"
			   "tuanany-completion-custom.el"
			   "tuanany-completion-embark.el"
			   "tuanany-completion-marginalia.el"
			   "tuanany-completion-orderless.el"
			   "tuanany-completion-vertico.el"
			   ;; ==== HELPER ====
			   "tuanany-helper-dumb-jump.el"
			   "tuanany-helper-flycheck.el"
			   "tuanany-helper-helpful.el"
			   "tuanany-helper-highlight-indentation.el"
			   "tuanany-helper-iedit.el"
			   "tuanany-helper-ispell.el"
			   "tuanany-helper-minibuffer.el"
			   "tuanany-helper-whitespace.el"
			   ;; "tuanany-helper-shrface.el"
			   ;; ==== TOOLS ====
			   ;; "tuanany-tools-evil.el"
			   "tuanany-tools-eww.el"
			   "tuanany-tools-magit.el"
			   "tuanany-tools-org-mode.el"
			   "tuanany-tools-proced.el"
			   "tuanany-tools-projectile.el"
			   "tuanany-tools-shell.el"
			   "tuanany-tools-skewer.el"
			   "tuanany-tools-tree-sitter.el"
			   ;; "tuanany-tools-browse-url.el"
			   ;; "tuanany-tools-impatient-mode.el"
			   ;; "tuanany-tools-prog-mode.el"
			   ;; ==== LANGUAGES ====
			   "tuanany-lang-clojure-mode.el"
			   "tuanany-lang-js2-mode.el"
			   "tuanany-lang-racket-mode.el"
			   "tuanany-lang-web-mode.el"
			   "tuanany-lang-yaml-mode.el"
			   ))

  (dolist (x tuanany-addons)
    (load x))

  ;; Enable these
  (dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
    (put c 'disabled nil))

  ;; And disable these
  (dolist (c '(eshell project-eshell overwrite-mode iconify-frame diary))
    (put c 'disabled t))
  (put 'eshell 'disabled nil)

  (defun tuanany--untabify-before-save ()
    "Convert tabs to space before saving the buffer."
    (when (and indent-tabs-mode
	       (< (buffer-size) 100000))  ; Skip files larger than 100KB
      (untabify (point-min) (point-max))))

  ;; Load custom-file if it exists
  (when (file-exists-p custom-file)
    (load custom-file))

  :custom-face
  (cursor ((t (:background "light goldenrod" :foreground "black"))))
  (web-mode-current-element-highlight-face ((t (:foreground "#ffffff" :underline "gold"))))

  ) ;; END EMACS-PACKAGE

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
