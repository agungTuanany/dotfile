;;; tuanany-helper.el --- Helper modules -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Created: 2026

;; Package-Requires: ((emacs "^25.1"))
;; Version: 0.1.0
;; Keywords: interface, helpers

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
;; interface for helpers.

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

(require 'tuanany-helper-dumb-jump)
(require 'tuanany-helper-epub)
(require 'tuanany-helper-flycheck)
(require 'tuanany-helper-goto-last-change)
(require 'tuanany-helper-highlight-indentation)
(require 'tuanany-helper-iedit)
(require 'tuanany-helper-ispell)
(require 'tuanany-helper-minibuffer)
(require 'tuanany-helper-multiple-cursors)
(require 'tuanany-helper-outline-indent)
(require 'tuanany-helper-pdf-tools)
(require 'tuanany-helper-whitespace)
(require 'tuanany-helper-which-key)
;; (require 'tuanany-helper-helpful)

(provide 'tuanany-helper)

;;; tuanany-helper.el ends here
