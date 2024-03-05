;;;; tuanany-ui-time.el --- Mode Line -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany 

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: ui, time

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

(use-package time
  :ensure t
  :custom
  (display-time-format " %a %e %b, %H:%M ")
  (display-time-interval 60)
  (display-time-default-load-average nil)
  (display-time-mail-function nil)
  (display-time-mail-directory nil)
  (display-time-use-mail-icon nil)
  (display-time-mail-string nil)
  (display-time-mail-face nil)
  :init
  (display-time-mode)
  )
