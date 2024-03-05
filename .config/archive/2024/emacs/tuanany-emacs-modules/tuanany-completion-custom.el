;;;; tuanany-completion-custom-.el --- Completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany 

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: completion

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

;;; Commentary:
;; This setup is mimicking from
;; Ref= - https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-emacs-modules/prot-emacs-completion-common.el'

;;;; Code:

;; General minibuffer settings
;;;; Minibuffer configurations
(setq completion-styles '(basic substring initials flex)) ; also see 'completion-category-overrides'
(setq completion-category-defaults nil)

(setq completion-category-overrides
      '((file (styles . (basic partial-completion)))
	(bookmark (styles .(basic substring)))
	(library (sytles . (basic substring)))
	(embark-keybinding (sytles .(basic substring)))
	(imenu (styles . (basic substring )))
	(consult-location (sytles . (basic substring )))
	(kill-ring (styles . (emacs22 )))
	(eglot (styles . (emacs22 substring )))))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq-default case-fold-search t)  ;; For general regexp

(setq enable-recursive-minibuffers t)
(setq resize-mini-windows t)
(setq minibuffer-default-prompt-ormat " [%s]")

(setq use-short-answers t)
(setq read-answer-short t) ; Also check `use-short-answers' for Emacs28
(setq echo-keystrokes 0.25)
(setq kill-ring-max 60) ; Keep it small

;; Do not allow the cursor to move inside the minibuffer prompt.
;; I gpt this from the documentation of Daniel Mendler's Vertico package
;; <https://github.com/minad/vertico>
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; Add prompt indicator to 'completing-read-multiple'. We display
;; ['completing-read-multiple': <separator>], e.g.,
;; ['completing-read-multiple: ,] if separator is comma. This is adapted from the
;; README of the 'vertico' package by Daniel Mendler. This function is tweak form
;; Protosilaos to propertize the segmets of the prompt. <prot-emacs-completion.el>.

(defun crm-indicator (args)
  "completing-read-multiple-indicator"
  (cons (format "[`crm-separator': %s] %s"
		(propertize
		 (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		  crm-separator)
		 'face 'error)
		(car args))
	(cdr args)))

(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Setting for the default completion UI.
(setq completion-show-help nil)
(setq completion-auto-help t)
(setq completion-auto-select nil)
(setq completions-detailed t) ; This variable make prompt more readable
(setq completion-show-inline-help nil)
(setq completions-max-height 15)
(setq completions-header-format
      (propertize "%s candidate\n" 'face 'font-lock-comment-face))
(setq completions-highlight-face 'completions-highlight)

(file-name-shadow-mode t)
(minibuffer-depth-indicate-mode t)
(minibuffer-electric-default-mode t)

;;;; `savehist' (minibuffer and related histories)
(setq savehist-file (locate-user-emacs-file "etc/history"))
(setq history-length 500)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables '(register-alist kill-ring))
(savehist-mode t)

;;;; `dabbrev' (dynamic world completion (Dynamic Abbrev Expansion))
(use-package dabbrev
  :ensure t
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-all-buffers nil)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p)
  (setq dabbrev-ignored-buffer-modes '(archive-mode-image-mode docview-mode pdf-view-mode))
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (setq dabbrev-upcase-means-case-search t)
  )

;;;; `abbrev' (abbreviation, else Abbrevs)
(setq abbrev-file-name (locate-user-emacs-file "etc/abbrevs_defs"))
(setq only-global-abbrevs nil)

;; message-mode derives from tex-mode, so we don't need a separate
;; hook for it
;(dolist (hook '(text-mode-hook prog-mode-hook git-commit-mode-hook))
;  (add-hook hook #'abbrev-mode))

;; By default, abbrev ask for confirmation on whether to use
;; `abbrev-file-name' to save abbrevations. I do not need that, nor do I want it.
;(remove-hook 'save-some-buffes-functions #'abbrev--possibly-save)

;; Orderless completion style (and tuanany.orderless.el)
;(use-package orderless
;:ensure t
;   :config
;   (setq orderless-component-separator " +")
;   (setq orderless-matching-styles
;	 '(orderless-prefixes orderless-regexp))
;   :custom
;   (completion-styles '(orderless basic))
;   (completion-category-overrides '((file (styles basic partial-completion)))))
