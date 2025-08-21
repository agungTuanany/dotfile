;;;; tuanany-lang-ruby-mode.el --- js2-mode Programming Languages Specified-*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2025
;; Version: 0.1.0
;; Keywords: ruby, programming language

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

;; Adds Ruby execution support to Org Babel.
;; Keeps Ruby logic separate from  org-mode core setup.

;;;; Code:

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode)
  )

(use-package robe
  :ensure t
  :hook (ruby-mode  . robe-mode))

(defun tuanany-org-babel-ruby-repl (body)
  "Send Ruby code BODY to an inf-ruby REPL and return the output."
  (let* ((buffer (inf-ruby-proc-buffer))
         (proc (get-buffer-process buffer)))
    (unless proc
      (user-error "No active Ruby REPL. Run `M-x inf-ruby' first."))
    (with-current-buffer buffer
      (goto-char (process-mark proc))
      (insert-body)
      (comint-send-input)
      (sleep-for 0.1) ;; give REPL time to respond
      (buffer-substring-no-properties
       (marker-position (process-mark proc))
       (point-max)))))

(defun tuanany-org-babel-execute:ruby (body params)
  "Custom executor for Ruby in Org-babel.
Executes BODY directly without wrapping in `def main`."
  (let ((tmpfile (org-babel-temp-file "ruby-")))
    (with-temp-file tmpfile
      (insert body))
    (org-babel-eval (format "ruby %s" tmpfile) "")))

;; Hook after org is fully loaded
(with-eval-after-load 'org
  (when (executable-find "ruby")
    ;; Force replace the default executor
    (fset 'org-babel-execute:ruby #'tuanany-org-babel-execute:ruby)))

(provide 'tuanany-lang-ruby-mode)
;;; tuanany-lang-ruby-mode.el ends here
