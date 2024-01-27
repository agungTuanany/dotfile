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
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the 'vc-handled-backends'.
(setq vc-handled-backends nil)
(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1024 1024 10)
		  gc-cons-percentage 0.2)))
