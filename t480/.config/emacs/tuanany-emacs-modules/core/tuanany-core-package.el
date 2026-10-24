;;; tuanany-core-package.el --- Core Package interfaces -*- lexical-binding: t; -*-

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
;; interface for core packages before all emacs packages get loaded.

;; https://stackoverflow.com/a/10091330/217812
;;
;; In summary,
;; [-] :custom is used for setting variables and faces associated with
;; the package, before package loaded; while
;; [-] :config is used for executing Emacs Lisp code to configure or
;; initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to

;; fully customize and configure a package to suit your needs.

;; The structure of this file is intentional:
;;
;; package archives
;; package priorities
;; package-initialize
;; use-package
;; no-littering

;;;; Code:
;; Note: the structure files, is matter. do not sort the lines alphabetically.


;;;; Package archives

;; Package archives are configured in early-init.el
;; they are kept there because package archive must be available before
;; package initialization

;;;; Package priorities

;; Package priorities are configured in early-init.el
;;
;; they are kept there because package archive must be available
;; before package initialization.

;;;; Package initialize

;; Package priorities are configured in early-init.el
;;
;; High number = higher priority when multiple archives provide the
;; same package.

;;;; Package initialization
(package-initialize)


;;;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; package.el
(use-package package
  :ensure nil
  :custom
  (package-install-upgrade-built-in t))

(use-package use-package
  :ensure t)


;;;; no-littering
(use-package no-littering
  :ensure t)


(provide 'tuanany-core-package)

;;; tuanany-core.el ends here
