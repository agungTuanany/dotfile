;;; early-init.el -- Early Init File _*_ lexical-binding: t _*_
;;; Commentary:

;; Copyright (C) 2021-2023 agung Tuanany 
;; Author: Agung Tuanany <agung.tuananydotgmail.com>
;; URL:
;; Package-Requires:

;; This file is nost part of GNU emacs.

;; This program is free software; you can redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software Foundation, either version 3 or the
;; License, or any later version.

;; This program is distributed in the hope it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied warranty of MERCHANTABILITY or FITNES FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have recieved a copy of the GNU General Public License along with this program.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; Temporarily increase the garbage collection threshold. These changes help
;; shave off about half a second of startup time.
;; https://akrl.sdf.org/#orgc15a10d
(setq garbage-collection-messages t)
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
      '(("gnu-elpa" . 3)
	("melpa" . 2)
	("org" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-install 'use-package)1)

(require 'use-package)
(setq use-package-always-ensure t)
