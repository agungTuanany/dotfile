;;;; tuanany-helper-dumb-jump.el --- dumb jump -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany 

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: jump, function definition, help

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
;; A basic 'go to' functionalit that work really well. No need for LSP any more.

;;;; Code:

(use-package dumb-jump
  :ensure t
  :defer
  :custom
  (dumb-jump-prefer-searcher 'ag)
  (dumb-jump-force-searcher 'ag)
  (dumb-jump-selector 'completing-read)
  (dumb-jump-default-project "~/work")
  :init
  (defun tuanany-filename/xref-filename-backend ()
    "Xref backend for jumping to HTML template definitions."
    (when (and (thing-at-point 'filename t) (string-suffix-p ".html" (thing-at-point 'filename t)))
      'tuanany-filename))

  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql tuanany-filename)))
    (thing-at-point 'filename t))

  (cl-defmethod xref-backend-definitions ((_backend (eql tuanany-filename)) identifier)
    (let ((path (cl-find-if (lambda (x) (string-match-p identifier x))
                            (projectile-project-files (projectile-project-root)))))
      (when path
        (list (xref-make identifier (xref-make-file-location (format "%s%s" (projectile-project-root) path) 1 0))))))

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (add-hook 'xref-backend-functions #'tuanany-filename/xref-filename-backend))
