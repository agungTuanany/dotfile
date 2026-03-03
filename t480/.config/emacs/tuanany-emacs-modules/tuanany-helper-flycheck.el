;;;; tuanany-helper-whitespace.el --- whitespace -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

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
(use-package flycheck
  :ensure t
  :init
  ;; Better UX defaults
  (setq flycheck-display-errors-delay 0.3
        flycheck-indication-mode 'right-fringe
        flycheck-emacs-lisp-load-path 'inherit)

  :config
  ;; Disable Flycheck completely in Org buffers
  (add-hook 'org-mode-hook
            (lambda ()
              (flycheck-mode -1)))

  ;; Define custom YAML checker
  (flycheck-define-checker yaml-gitlab
    "YAML syntax checker using yamllint with GitLab schema."
    :command ("yamllint" "-d" "{extends: gitlab}" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column
              ": [warning] " (message) line-end)
     (error line-start (file-name) ":" line ":" column
            ": [error] " (message) line-end))
    :modes (yaml-mode))

  (add-to-list 'flycheck-checkers 'yaml-gitlab)

  :bind
  (:map flycheck-mode-map
        ("C-c C-n" . flycheck-next-error)
        ("C-c C-p" . flycheck-previous-error)))

;;; tuanany-helper-flycheck.el ends here
