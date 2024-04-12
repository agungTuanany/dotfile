;;;; tuanany-tools-skewer.el --- skewer -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: live-sever

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
;; Dependencies:
;;   - simple-httpd
;;   - js2-mode

;;;; Code:
(use-package skewer-mode
  :ensure t
  :config
  (define-key skewer-mode-map (kbd "C-c C-k") 'skewer-repl)
  (define-key skewer-mode-map (kbd "C-c C-c") 'skewer-eval-defun)
  (define-key skewer-mode-map (kbd "C-c C-z") 'skewer-eval-region)
  :hook
  (js2-mode . skewer-mode)
  (css-mode . skewer-css-mode)
  (html-mode . skewer-html-mode)
  (web-mode . skewer-html-mode))

;; (use-package simple-httpd
;;   :ensure t
;;   :config
;;   (setq httpd-root "/var/www")
;;   (httpd-start))

;;; tuanany-tools-skewer.el ends here
