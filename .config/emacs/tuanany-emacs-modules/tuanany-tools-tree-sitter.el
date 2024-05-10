;;;; tuanany-helper-tree-sitter-.el --- tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: tree-sitter, treesit

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
;; Configuration for tree-sitter

;;;; Code:

(use-package tree-sitter
  :hook
  ((
    css-mode
    elisp-mode
    js-mode
    json-mode
    php-mode
    ruby-mode
    rust-mode
    sh-mode
    terraform-mode
    typescript-mode
    yaml-mode
    ) . tuanany-tree-sitter-mode-enable)
  :preface
  (defun tuanany-tree-sitter-mode-enable ()
    (tree-sitter-mode t))
  :defer t)

(use-package tree-sitter-langs
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; tuanany-tools-tree-sitter.el ends here
