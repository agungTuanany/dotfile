;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Created: Fri Dec 17 22:11:36 2021 +0700

;; Package-Requires: ((Emacs "25.1"))
;; Version: 0.1.0
;; Keywords: init file, key mapping, modular file, custom config.

;;;; License: GPL-3.0-or-later
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

;; In summary,
;; [-] :custom is used for setting variables and faces associated with the
;; package, before package loaded, only for "defcustom" variables not all
;; variables; while
;; [-] :config is used for executing Emacs Lisp code to
;; configure or initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to
;; fully customize and configure a package to suit your needs.

;; Thanks to:
;;
;; - James Cherti - https://github.com/jamescherti/minimal-emacs.d
;; - Lars Tveito https://github.com/larstvei/dot-emacs

;;; Code:

;; Recommended to have this at the top
;; Source: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; Emacs 29 support for tree-sitter built in.

(require 'package)

(require 'use-package)
(setopt use-package-always-ensure t)

;;; Module search path
(let ((root (locate-user-emacs-file "tuanany-emacs-modules")))
  (dolist (dir (directory-files root t "^[^.]" t))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;;; Load configuration
(require 'tuanany-core)
(require 'tuanany-ui)
(require 'tuanany-completion)
(require 'tuanany-tools)
(require 'tuanany-languages)
(require 'tuanany-helper)

;; (message "display-line-numbers-type = %S"
;;          display-line-numbers-type)

;;; init.el ends here
