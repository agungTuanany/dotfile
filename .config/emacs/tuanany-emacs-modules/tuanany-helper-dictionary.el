;;;; tuanany-helper-dictionary.el --- dictionary -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany 

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: helper package, dictionary 

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
;; Look up word at point using dict.org in readme/text/org-mode buffers
;; Ref: https://github.com/gopar/.emacs.d?tab=readme-ov-file#spelling

;;;; Code:

(use-package dictionary
  :defer
  :ensure nil
  :bind (:map text-mode-map
              ("C->" . dictionary-lookup-definition)
         :map org-mode-map
              ("C->" . dictionary-lookup-definition)
         :map dictionary-mode-map
              ("C->" . dictionary-lookup-definition))
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*" display-buffer-in-side-window
                 (side . left)
                 (window-width . 50)))
  :custom
  (dictionary-server "dict.org"))
