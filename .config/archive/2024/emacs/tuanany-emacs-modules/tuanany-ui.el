;;;; tuanany-ui.el --- User Interface-*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany 

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: faces, programming language

;;;; Package-Requires:

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

;;;; Code:

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  (setq-default doom-theme-enable-bold t
		doom-theme-enable-italic t)
  (load-theme 'doom-monokai-octagon t)
  (set-cursor-color "yellow"))

(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode))

(use-package paredit
  :hook
  ((racket-mode . paredit-mode)
   (racket-repl-mode . paredit-mode)))

;;;; Display time
(setq display-time-format " %a %e %b, %H:%M ")
(setq display-time-interval 60)
(setq display-time-default-load-average nil)
(setq display-time-mail-function nil)
(setq display-time-mail-directory nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)

;; I don't need the load average and the mail indicator, so let this
;; be simple:
;(setq display-time-string-forms
;      '((propertize
;	 (format-time-string display-time-format now)
;	 'face 'display-time-date-and-time
;	 'help-echo (format-time-string "%a %b %e, %Y" now))
;	" "))

(display-time-mode 1)

;;;; `proced' (process monitor, similar to `top')
(setq proced-auto-update-flag t)
(setq proced-enable-color-flag t) ; Emacs 29
(setq proced-auto-update-interval 5)
(setq proced-descend t)
(setq proced-filter 'user)

;;;; Shell (M-x shell)
(setq shell-command-prompt-show-cwd 1) ; Emacs 27.1
(setq ansi-color-for-comint-mode t)
(setq shell-input-autoexpand 'input)
(setq shell-highlight-undef-enable 1) ; Emacs 29.1
(setq shell-has-auto-cd nil) ; Emacs 29.1
;;(setq shell-get-old-input-include-continuation-lines) ; Emacs 30.1
(setq shell-kill-buffer-on-exit 1) ; Emacs 29.1
(setq shell-completion-fignore '("~" "#" "%"))
(setq-default comint-scroll-to-bottom-on-input t)
(setq-default comint-scroll-to-bottom-on-output nil)
(setq-default comint-input-autoexpand 'input)
(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 9999)
(setq comint-completion-autolist 1)
(setq comint-input-ignoredups 1)
(setq tramp-default-remote-shell "/bin/bash")

(setq shell-font-lock-keywords
      '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
	("^[^ \t\n]+:.*" . font-lock-string-face)
	("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))
