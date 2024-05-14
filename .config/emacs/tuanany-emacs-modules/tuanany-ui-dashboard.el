;;; tuanany-ui-dashboard.el -- Dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: dashboard

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

(use-package dashboard
  :config
  (setq dashboard-startup-banner 'ascii)
  (setq dashboard-banner-logo-title "Hello Agung Jangan Malas, ingat Aly dan Istrimu")
  (setq dashboard-footer-messages '("Take small steps and don't get discouraged. Good luck"
                                    "an idiot admires complexity, a genius admires simplicity"
                                    "Before the endgames, the Gods have placed the middle game, -Siegbert Tarrasch"
                                    "Don't tell me what the code does, show me what the code does, -Anonymous"))
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook)
  )

;;; tuanany-ui-dashboard.el ends here
