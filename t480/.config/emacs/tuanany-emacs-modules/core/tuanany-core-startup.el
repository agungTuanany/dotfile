;;; tuanany-core-startup.el --- Core Package interfaces -*- lexical-binding: t; -*-

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
;; Essential persistent state settings.g

;; https://stackoverflow.com/a/10091330/n217812
;;
;; In summary,
;; [-] :custom is used for setting variables and faces associated with
;; the package, before package loaded; while
;; [-] :config is used for executing Emacs Lisp code to configure or
;; initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to
;; fully customize and configure a package to suit your needs.

;;;; Code:
;; Note: the structure files, is matter. do not sort the lines alphabetically.
;; Categories
;; - Startup behavior,
;; - performance,
;; - garbage collection,and
;; - persistent state.


;;;; Startup and performance
(setopt inhibit-compacting-font-caches t)

(setopt load-prefer-newer t)

;;;; Startup screen

;; Suppress the default startup screen
(setopt inhibit-startup-screen t)

;; Start with an empty *scratch* buffer
(setopt initial-scratch-message "")

;; Suppress the startup echo-area message for the current user
(setopt inhibit-startup-echo-area-message user-login-name)

;; Leave initial-buffer-choice at its default so startup packages,
;; such as Dashboard, can manage the initial display.


;;;; Garbage collection

;; A higher threshold can reduce pauses cause by garbage collection
;; during startup. The post-startup value are reasonable starting
;; points, not universal performance optimization

;; Reduce garbage collection during startup
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

(defun tuanany--restore-gc-after-startup ()
  "Restore normal garbage-collection settings after startup."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1))

;; Restore normal values after startup
(add-hook 'emacs-startup-hook #'tuanany--restore-gc-after-startup)


;;;; Persistent state directory

(defvar tuanany/etc-directory
  (expand-file-name "etc/" user-emacs-directory)
  "Directory for persistent Emacs state files")

(make-directory tuanany/etc-directory t)

;;; Recent files

;; Store recent-file history in the state directory.
(setopt recentf-save-file
        (expand-filename "recentf" tuanany/etc-directory))

(recentf-mode 1)

;;; Command history

;; Store minibuffer command history in the state directory.
(setopt savehist-file
        (expand-file-name "savehist" tuanany/etc-directory))

(savehist-mode 1)


;;; Position history

;; Remember the last visited position in files.
(setopt save-place-file
        (expand-file-name "saveplace" tuanany/etc-directory))

(save-place-mode 1)


(provide 'tuanany-core-startup)

;;; tuanany-core-startup.el ends here
