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

;;;; Persistent state directory

(defvar tuanany/etc-directory
  (expand-file-name "etc/" user-emacs-directory)
  "Directory for persistent Emacs state files")

(make-directory tuanany/etc-directory t)

;;;; Recent files
;; recentf-mode
(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file
   (expand-file-name "recentf" tuanany/etc-directory))
  :init
  (recentf-mode 1))

;;;; Minibuffer history
;; savehist-mode
(use-package savehist
  :ensure nil
  :custom
  (savehist-file
   (expand-file-name "savehist" tuanany/etc-directory))
  :init
  (savehist-mode 1))

;;;; Last editing position
;; save-place-mode

(use-package saveplace
  :ensure nil
  :custom
  (save-place-file
   (expand-file-name "saveplace" tuanany/etc-directory))
  :init
  (save-place-mode 1))

;; (make-directory
;;  (expand-file-name "backups/" tuanany/etc-directory))

;;;; Garbage collection
;; A higher threshold can reduce pauses cause by garbage collection
;; during startup. The post-startup value are reasonable starting
;; points, not universal performance optimization

;; Reduce garbage collection during startup
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

;; Restore normal values after startup
(add-hook 'emacs-startup-hook
      (lambda ()
        (setq gc-cons-threshold (* 64 1024 1024)
          gc-cons-percentage 0.1)))

;;;; native-comp
;; useful distinct with:
;; 'native-comp is the native compilation subsystem.
;; 'native-comp-asnyc-report-warnings-errors, controls how asynchronous native-compilation warning are reported
;; 'native-comp-asyc is related internals can Vary by emacs version and build

;; startup screen
(setopt inhibit-startup-screen t)
(setopt initial-scratch-message "")

;; after-init-hook
;; emacs-startup-hook

(provide 'tuanany-core-startup)

;;; tuanany-core-startup.el ends here
