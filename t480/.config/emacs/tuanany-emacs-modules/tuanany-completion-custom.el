;;;; tuanany-completion-custom.el --- completion custom -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: custom completion

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

(use-package completion
  :ensure t
  :custom
  (completion-cycle-threshold 3)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (tab-always-indent 'complete)

  (completion-category-overrides
   '((file (styles . (basic partial-completion)))
     (bookmark (styles .(basic substring)))
     (library (sytles . (basic substring)))
     (embark-keybinding (sytles .(basic substring)))
     (imenu (styles . (basic substring )))
     (consult-location (sytles . (basic substring )))
     (kill-ring (styles . (emacs22 )))
     (eglot (styles . (emacs22 substring )))))

  (completion-ignore-case t)
  ;; :config
  )
;;;; (dynamic world completion (Dynamic Abbrev Expansion))
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
;;; tuanany-completion-custom.el ends here
