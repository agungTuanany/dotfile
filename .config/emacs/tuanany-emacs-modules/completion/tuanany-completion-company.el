;;;; tuanany-company-completion.el --- Completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany 

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: Completion

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

(use-package company
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 4)
  :hook
  ((emacs-lisp-mode . (lambda ()
			(setq-local company-backends '(company-elisp)))))
  (emacs-lisp-mode . company-mode)
  )
