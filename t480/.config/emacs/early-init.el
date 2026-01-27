;;;; early-init.el --- Emacs Early Init File -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: init file, early init file

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

;;;; Code:

;; Backup of `gc-cons-threshold' and `gc-cons-percentage' before startup.
(defvar minimal-emacs--backup-gc-cons-threshold gc-cons-threshold)
(defvar minimal-emacs--backup-gc-cons-percentage gc-cons-percentage)

;; Temporarily increase the garbage collection threshold. These changes help
;; shave off about half a second of startup time.
;; https://akrl.sdf.org/#orgc15a10d
;; (setq garbage-collection-messages t)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the 'vc-handled-backends'.
(setq vc-handled-backends nil)
(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 10)
                  gc-cons-percentage 0.2)))

;;;; PACKAGES
;; Presetup package sources
(require 'package)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 1)
        ("melpa" . 2)
        ("org" . 3)))

;; Initialize package system but don't activate packages yet
(setq package-enable-at-startup nil)
(package-initialize)

;; Configure use-package (built-in)
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
;;; early-init.el ends here
