;;;; tuanany-helper-whitespace.el --- whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: flycheck

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

(use-package flycheck
  :ensure
  :init (global-flycheck-mode)
  :config
  (flycheck-define-checker yaml-gitlab
    "A YAML syntax checker using yamllint with Gitlab CI schema"
    :command ("yamllint" "-d" "{extends: gitlab}" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": [warning] " (message) line-end)
     (error line-start (file-name) ":"line ":" column ": [error] " (message) line-end))
    :modes (yaml-))
  :hook ((prog-mode . flycheck-mode))
  :bind (:map flycheck-mode-map
	      ("C-c C-n" . flycheck-next-error)
	      ("C-c C-p" . flycheck-previous-error))
  :custom
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-to-list 'flycheck-checkers 'yaml-gitlab)
  )

;;; tuanany-helper-flycheck.el ends here
