;; Copyright (C) 2021-2023 agung Tuanany 
;; Author: Agung Tuanany <agung.tuananydotgmail.com>
;; Filename: tuanany-mode-line.el
;; URL:
;; Package-Requires:

;; This file is nost part of GNU emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 or the License, or any later version.

;; This program is distributed in the hope it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNES FOR
;; A PARTICULAR PURPOSE. See the

;; GNU General Public License for more details.

;; You should have recieved a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO: Create your own modeline style from scratch
;; today I just use doom-modeline to just simplify my work
(use-package doom-modeline
  :config
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-total-line-number t)
  ;; (setq doom-modeline-position-line-format '("L%l"))
  ;; (setq doom-modeline-position-column-format '("%l:%c"))
  (setq column-number-mode t)
  :init
  (doom-modeline-mode 1))
