;;; tuanany-core-editing.el --- Core Editing interfaces -*- lexical-binding: t; -*-
;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Created: 2026

;; Package-Requires: ((emacs "^25.1"))
;; Version: 0.1.0
;; Keywords: interface, core modules

;;;; License:
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 or the License, or any later version.

;; This program is distributed in the hope it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITINES FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Commentary:
;; All core editing config

;; https://stackoverflow.com/a/10091330/217812
;;
;; In summary,
;; [-] :custom is used for setting variables and faces associated with
;; the package, before package loaded; while
;; [-] :config is used for executing Emacs Lisp code to configure or
;; initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to
;; fully customize and configure a package to suit your needs.

;;;; Code:
;; Categories:
;;  - Text editing,
;;  - indentation,
;;  - navigation,
;;  - buffers,
;;  - folding, and
;;  - keybindings


;;;; Text and indentation

(setopt
 indent-tabs-mode nil
 tab-width                         4             ;; tabs are evil
 sentence-end-double-space         nil           ;; End sentence with 1 space not 2
 fill-column                       80
 require-final-newline             t
 narrow-to-defun-include-comments  t
 x-underline-at-descent-line       t
 backward-delete-char-untabify-method 'untabify)

;; Prefer wrapped lines over truncated lines by default
(setq-default truncate-lines nil)


;;;; Parenthesis matching

(setopt
 show-paren-delay 0.1
 show-paren-when-point-inside-paren t
 show-paren-when-point-in-periphery t
 show-paren-style 'expression)

(show-paren-mode 1)


;;;; Editing behavior

;; Indentation and paired delimiters
(electric-indent-mode 1)
(electric-pair-mode 1)

;; Replace selected text when typing
(delete-selection-mode 1)

;; Configure deletion and kill-ring behavior here.
;; kill-ring-max
;; kill-do-not-save-duplicates

;; Configure undo limits here.
;; undo-limit
;; undo-strong-limit
;; undo-outer-limit

;;;; Subword navigation

;; Treat CamelCase words as separate subwords
;; NOTE: It's make emacs read-only-mode!!
;; (global-subword-mode 1)


;;;; Navigation and search

(setopt switch-to-buffer-obey-display-action t)

;; Configure these only if needed.
;; switch-to-buffer-in-dedicated-window
;; outline-blank-line
;; search-invisible
;; imenu-max-item-length


;;;; Buffer management

;; ibuffer-formats
;; kill-buffer-query-functions
;; uniquify-buffer-name-style


;;;; Abbrev

;; Configure abbrev-mode separately when needed.

;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
;; when no `ispell' dictionary is set.


;;;; Ispell and Flyspell

;; Ispell
(setopt
 text-mode-ispell-word-completion   nil
 ispell-silently-savep              t)

;;; flyspell
;; Enable Flyspell in the relevant language or text modules,
;; where the spelling program and dictionary can be configured
(setopt flyspell-issue-welcome-flag nil)

;; Improves flyspell performance by preventing messages from being displayed for
;; each word when checking the entire buffer.
(setopt flyspell-issue-message-flag nil)

;; Enable Flyspell in Language or text modules where appropriate.


;;;; Interactive evaluation

;; eval-expression-print-length
;; eval-expression-print-level


;;; Folding

(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Enable outline-indent-minor-mode in relevant model when needed
;; outline-indent-minor-mode

;; Define tuanany-toggle-fold here if you want a custom folding command
;; tuanany-toggle-fold

;;;; Whitespace

;; Configure whitespace visualization, but do not enable it globally
(setopt
 whitespace-style
 '(face tabs trailing spaces space-before-tab))

;;;; Before-save cleanup

(defun tuanany--cleanup-buffer-before-save()
  "Clean tabs and trailing whitespace before saving the buffer."
  (unless (derived-mode-p 'makefile-mode 'makefile-gmake-mode)
    (untabify (point-min) (point-max)))
  (delete-trailing-whitespace))

(add-hook 'before-save-hook
          #'tuanany--cleanup-buffer-before-save)

;;;; Global keybindings

;; Remove default bindings.
(keymap-global-unset "C-x C-c")
(keymap-global-unset "C-x C-o")

;; Set custom bindings.
(keymap-global-set "C-c f"       #'find-name-dired)
(keymap-global-set "C-x C-c C-c" #'save-buffers-kill-emacs)
(keymap-global-set "C-x C-o"     #'other-window)
(keymap-global-set "C-x M-f"     #'recentf-open-files)
(keymap-global-set "C-x C-b"     #'ibuffer)


(provide 'tuanany-core-editing)

;;; tuanany-core-edting.el ends here
