;;;; tuanany-helper-minibuffer.el --- minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: multiple occurrences in the same way simultaneously


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
;; Try to check the reference below, it's a great to start.
;; Ref: - http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs/
;;      - https://github.com/redguardtoo/emacs.d
;;      - https://github.com/gopar/.emacs.d?tab=readme-ov-file#spelling

;;;; Code:

;; It may also be wise to raise gc-cons-threshold while the minibuffer is active,
;; so the GC doesn't slow down expensive commands (or completion frameworks, like
;; helm and ivy. The following is taken from doom-emacs
(use-package minibuffer
  :ensure nil
  :custom
  (completion-styles '(initials partial-completion flex)))

;;; tuanany-helper-minibuffer.el ends here
