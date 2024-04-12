;;;; tuanany-ui-doom-modeline.el --- doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: modeline

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

(use-package doom-modeline
  :config
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-time-live-icon nil)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-total-line-number t)
  ;; (setq doom-modeline-position-line-format '("L%l"))
  ;; (setq doom-modeline-position-column-format '("%l:%c"))
  (setq column-number-mode t)
  :init
  (doom-modeline-mode 1))

;;; tuanany-ui-doom-modeline.el ends here
