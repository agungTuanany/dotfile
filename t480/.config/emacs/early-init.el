;;;; early-init.el --- Emacs Early Init File -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Created: Fri Dec 17 22:11:36 2021 +0700

;; Package-Requires: ((Emacs "25.1"))
;; Version: 0.1.0
;; Keywords: init file, key mapping, modular file, custom config.

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

;; early-init.el is prepares Emacs,

;; - one require 'package
;; - one place defining archives
;; - no package-initialize

;;;; Code:

;; Faster startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(setq package-enable-at-startup nil)
(setq vc-handled-nbackends nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024)
		  gc-cons-percentage 0.1)))

;;;; Package bootstrap
(require 'package)

(setopt
 package-archives
 '(("gnu"   . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ;; ("org"   . "https://orgmode.org/elpa/") ;;  Org ELPA is discontinued, will close before Org 96.
   )


 package-archive-priorities
 '(("gnu"   . 1)
   ("melpa" . 2)
   ("org"   . 3))

;; Disable automate package initialization
 package-enable-at-startup nil)

(add-hook 'package-menu-mode-hook #'hl-line-mode)
