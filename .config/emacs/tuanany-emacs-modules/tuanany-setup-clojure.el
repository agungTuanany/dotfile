;;; tuanany-setup-clojure.el -- UI File _*_ lexical-binding: t _*_
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

(use-package clojure-mode
  :hook
  (subword-mode
   pareedit-mode))

(use-package cider
  :bind-keymap
  ("C-c u" . cider-user-ns)
  :config
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.config/emacs/etc/cider-history")
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-repl-wrap-history t)
  )

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))
