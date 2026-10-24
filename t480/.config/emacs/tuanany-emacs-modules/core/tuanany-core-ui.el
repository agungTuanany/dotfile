;;; tuanany-core-ui.el --- early UI setup  -*- lexical-binding: t; -*-

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
;; interface for core packages before all emacs packages get loaded.

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
;; Note: the structure files, is matter. do not sort the lines alphabetically.


;;; Three categories to config in core-ui
;; display
;; faces
;; window


;;;; Frame and window appearance
;; Hide unnecessary UI elements.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Set frame appearance.
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(left-fringe . 6))
(add-to-list 'default-frame-alist '(right-fringe . 6))

;;;; Font
;; Source Code Pro, 14 pt.
(add-to-list 'default-frame-alist
         '(font . "Source Code Pro-14"))

(set-face-attribute 'default nil
            :family "Source Code Pro"
            :height 140)

;;;; Display
(setopt display-line-numbers-type    'relative)
(setopt display-line-numbers-width   3)
(setopt display-line-numbers-widen   t)

(global-display-line-numbers-mode 1)

;;; Cursors
;; cursor-type
;; x-stretch-cursor


;;; Windows
;; split-width-threshold
;; split-height-threshold
;; display-buffer-alist
;; window-sides-slots


;;; Theme-independent faces
;; cursor
;; line-number-current-line
;; font-lock-comment-face
;; whitespace-tab


;;; Scrolling
;; scroll-margin
;; scroll-step
;; scroll-conservatively
;;; Fringe
;; fringe-mode
;; indicate-buffer-boundaries

(provide 'tuanany-core-ui)

;;; tuanany-core-ui.el ends here
