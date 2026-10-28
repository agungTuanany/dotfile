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

;; Categories:
;; Coding systems, file persistence, recovery, confirmations,
;; file utilities, external integrations, bookmarks, and debugging.


;;;; Coding systems

;; Prefer UTF-8 for text and file operations.
(prefer-coding-system 'utf-8)

;; Prefer loading newer source files over older byte-compiled files.
(setopt load-prefer-newer t)


;;;; Custom file

;; Store Emacs Custom generated settings separately from init files.
(setq custom-file
      (expand-file-name "custom.el" tuanany/etc-directory))'

; Load the custom file only when it exists
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;;; Backup files

;; Store backup files under \~/.config/emacs/etc/backups/
(setopt
 make-backup-files t
 backup-inhibited nil
 backup-by-copying t
 backup-directory-alist
 `(("." . , (expand-file-name "backups/" tuanany/etc-directory)))
 version-control t
 kept-new-versions 5
 kept-old-versions 2
 delete-old-versions t)

(make-directory
 (expand-file-name "backups/" tuanany/etc-directory)
 t)


;;;; Auto-save and lockfiles

;; Store auto-save file under ~/.config/emacs/etc/autosave/

(setopt
 auto-save-file-name-transforms
 `((".*" ,(expand-file-name "autosave/" tuanany/etc-directory) t))
 auto-save-list-file-prefix
 (expand-file-name "auto-save-list/.saves-" tuanany/etc-directory))

(make-directory
 (expand-file-name "autosave/" tuanany/etc-directory) t)

(make-directory
 (expand-file-name "auto-save-list/" tuanany/etc-directory) t)

;; Preserve Emacs's default lockfile behavior to warn about simultaneous
;; editing.
(setopt create-lockfiles t)


;;;; Confirmation and safety

;; use y/n instead of yes/no
(setopt use-short-answers t)

;; Ask before killing process associated with buffers
(setopt confirm-kill-processes t)

;; Move deleted files to the system trash when supported
(setopt delete-by-moving-to-trash t)


;;;; File and directory utilities

;;; Buffers names

;; Distinguish buffers with identical filenames by their paths.
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/"))

;;; Automatic file updates

;;; Auto-revert
(global-auto-revert-mode 1)


;;;; Dired

(setopt
 dired-listing-switches          "-alhv --group-directories-first"
 dired-free-space                nil
 dired-dwim-target               t  ; Propose a target for intelligent moving/copying
 dired-deletion-confirmer        'y-or-n-p
 dired-filter-verbose            nil
 dired-recursive-deletes         'top
 dired-recursive-copies          'always
 dired-vc-rename-file            t
 dired-create-destination-dirs   'ask)

;; Suppress Dired buffer kill prompt for deleted dirs
(setopt dired-clean-confirm-killing-deleted-buffers nil)

;; This is a higher-level predicate that wraps `dired-directory-changed-p'
;; with additional logic. This `dired-buffer-stale-p' predicate handles remote
;; files, wdired, unreadable dirs, and delegates to dired-directory-changed-p
;; for modification checks.
(setopt
 auto-revert-remote-files  nil
 dired-auto-revert-buffer  'dired-buffer-stale-p)

;; dired-omit-mode
(setopt
 dired-omit-verbose  nil
 dired-omit-files    (concat "\\`[.]\\'"))

(setopt
 ls-lisp-verbosity   nil
 ls-lisp-dirs-first  t)


;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(setopt
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

;;;; Tree-sitter

;; Add custom grammar locations here if you build grammars
;; outside Emacs's default search paths.
;; Example:
;; (add-to-list 'treesit-extra-load-path
;;              (expand-file-name "tree-sitter/" user-emacs-directory))


;;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; ediff-window-setup-function
;; ediff-split-window-function


;;;; TRAMP

(setopt
 tramp-verbose                             1
 tramp-completion-reread-directory-timeout 50
 remote-file-name-inhibit-cache            50)


;;;; Bookmarks

;; Save bookmarks immediately after change
(setopt bookmark-save-flag 1)


;;;; Debugging

(setopt
 debugger-stack-frame-as-list           t
 toggle-debug-on-error                  t)

(provide 'tuanany-core-system)

;;; tuanany-core-system.el ends here
