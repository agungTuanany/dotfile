;;; tuanany-setup-racket.el -- PROGRAMMING LANGUAGES SPECIFIED _*_ lexical-binding: t _*_
;; Copyright (C) 2021-2023 agung Tuanany 
;; Author: Agung Tuanany <agung.tuananydotgmail.com>
;; URL:
;; Package-Requires:

;; This file is nost part of GNU emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 or the License, or any later version.

;; This program is distributed in the hope it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNES FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have recieved a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:


;; TODO
;(setq-default cursor-type  '(hbar . 3))

;;; Code:

(use-package racket-mode
  :hook
  (racket-mode . racket-xp-mode)
  (racket-mode . company-mode)
  (racket-repl-mode . company-mode)
  )

;;; This is the binary name of my scheme implementation
(setq scheme-program-name "mzscheme")

;; (use-package quack
;;      :hook
;;      (quack-mode racket-mode))

;; (require 'quack)