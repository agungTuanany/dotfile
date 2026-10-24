;;; tuanany-core-system.el --- Core system interfaces -*- lexical-binding: t; -*-

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
;; interface for core system before all emacs packages get loaded.

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

;; custom-file
(setq custom-file
      (expand-file-name "custom.el" tuanany/etc-directory))'

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Backup files
(setopt
 backup-directory-alist
 `(("." . , (expand-file-name "backups/" tuanany/etc-directory)))
 backup-by-copying t
 version-control t
 kept-new-versions 5
 kept-old-versions 2
 delete-old-versions t)

;; Auto-save
(setopt
 auto-save-file-name-transforms
 `((".*" ,(expand-file-name "autosave/" tuanany/etc-directory) t))
 auto-save-list-file-prefix
 (expand-file-name "auto-save-list/.saves-" tuanany/etc-directory))

(make-directory
 (expand-file-name "autosave/" tuanany/etc-directory) t)

(make-directory
 (expand-file-name "auto-save-list/" tuanany/etc-directory) t)

;; Lockfiles

;; Preserve Emacs's default lockfile behavior
(setopt create-lockfiles t)


;;;; Confirmation prompts

;; use y/n instead of yes/no
(setopt use-short-answers t)

;; Ask before killing process associated with buffers
(setopt confirm-kill-processes t)


;;;; Trash

;; Move deleted files to the system trash when supported
(setopt delete-by-moving-to-trash t)


;;;; Uniquify buffers names

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/"))

;;;; Tree-sitter

;; Add custom grammar locations here if you build grammars
;; outside Emacs's default search paths.
;; Example:
;; (add-to-list 'treesit-extra-load-path
;;              (expand-file-name "tree-sitter/" user-emacs-directory))

(provide 'tuanany-core-system)

;;; tuanany-core-system.el ends here
