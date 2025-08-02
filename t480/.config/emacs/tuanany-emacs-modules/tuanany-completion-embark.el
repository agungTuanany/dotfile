;;;; tuanany-completion-embark-.el --- completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: completion

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

(use-package embark
  :ensure t
  :bind
  (("C-c C-." . embark-act)      ; Main embark action
   ("C-c C-;" . embark-dwin)     ; Do-What-I-Mean
   ("C-h B"   . embark-bindings) ; Sow Key bindings
   )
  :init
  ;; Show Embark actions via which-key
  (setq embark-action-indicator
	(lambda (map)
	  (which-key--show-keymap "Embark" map nil nil 'no-paging)
	  #'which-key--hide-popup-ignore-command))
  (setq embark-become-indicator embark-action-indicator)

  :config
  ;; Better completion targets
  (add-to-list 'embark-target-finders 'consult--fast-command)
  (add-to-list 'embark-target-finders 'consul-imenu-target)

  (add-hook 'embark-collect-mode-hook #'marginalia-mode))


(use-package embark-consult
  :ensure t
  :after (embark consult)
  :config
  ;; Enable all the consult integrations
  (embark-consul-setup)

  ;; Preview for embark-consult
  (setq embark-consult-preview-key "M-."))

;; Integration with consult
(with-eval-after-load 'consult
  (defvar consult--source-embark
    (list :name "Embark Action"
	  :category 'command
	  :items (lambda ()
		   (when-let ((act (embark--action-target)))
		     (embark--action-bindings act))))))

;;; tuanany-completion-embark.el ends here
