;;; tuanany-setup-html.el -- UI File _*_ lexical-binding: t _*_
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


;; SUMMARY:
;; There is three option for html, use 'ac-html.el or 'company-web.el or
;; 'web-mode.el. My first attempt to try 'web-mode.el

;;; Code:

(use-package web-mode
  :config

  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; Enable / Disable features
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-comment-interpolation t)
  ;; (setq web-mode-enable-current-column-highlight t)
  )

;; (use-package clojure-mode
;;   :hook
;;   (subword-mode
;;    pareedit-mode))

;; (use-package cider
;;   :bind-keymap
;;   ("C-c u" . cider-user-ns)
;;   :config
;;   (setq cider-show-error-buffer t)
;;   (setq cider-auto-select-error-buffer t)
;;   (setq cider-repl-history-file "~/.config/emacs/etc/cider-history")
;;   (setq cider-repl-pop-to-buffer-on-connect t)
;;   (setq cider-repl-wrap-history t)
;;   )

;; (defun cider-user-ns ()
;;   (interactive)
;;   (cider-repl-set-ns "user"))
