;;;; tuanany-completion-vertico.el --- Programming Languages Specified-*- lexical-binding: t -*-

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
;; https://stackoverflow.com/a/10091330/217812
;;
;; In summary,
;; [-] :custom is used for setting variables and faces associated with
;; the package, before package loaded; while
;; [-] :config is used for executing Emacs Lisp code to configure or
;; initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to
;; fully customize and configure a package to suit your needs.

;;;; Code:

(use-package vertico
  :ensure t
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (setq vertico-scroll-margin 0)
  (setq vertico-count 5)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  (defvar vertico-multiform-minimal
    '(unobtrusive
      (vertico-flat-format . ( :multiple ""
                               :single ""
                               :prompt ""
                               :separator ""
                               :ellipsis ""
                               :no-match ""))))
  :init (vertico-mode 1)
  )

(use-package vertico-multiform
  :ensure nil
  :hook (after-init . vertico-multiform-mode)
  :init
  (setq vertico-multiform-commands
        '((consult-line (:not posframe))
          ;; (tuanany-consult-line (:not posframe))
          (consult-ag (:not posframe))
          (consult-grep (:not posframe))
          (consult-imenu (:not postframe))
          (xref-find-definitions (:not posframe))
          (t posframe)))
  )

(use-package vertico-posframe
  :ensure t
  :custom(vertico-posframe-parameters
          '((left-fringe . 8)
            (right-fringe . 8)))
  )

;;; tuanany-completion-vertico.el ends here
