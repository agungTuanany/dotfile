;;; tuanany-helper-kirigami.el --- Structural editing with kirigami -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Created: 2026

;; Package-Requires: ((emacs "^25.1"))
;; Version: 0.1.0
;; Keywords: Whitespace

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
(use-package kirigami
  :ensure t

  ;; Activate in Lisp-related modes
  ;; :hook
  ;; ((emacs-lisp-mode . kirigami-mode)
  ;;  (lisp-mode       . kirigami-mode)
  ;;  (clojure-mode    . kirigami-mode)
  ;;  (scheme-mode     . kirigami-mode))

  :bind-keymap

  (("C-c z o"   .   kirigami-open-fold)       ; Open fold at point
   ("C-c z O"   .   kirigami-open-fold-rec)   ; Open fold recursively
   ("C-c z r"   .   kirigami-open-folds)      ; Open all folds
   ("C-c z c"   .   kirigami-close-fold)      ; Close fold at point
   ("C-c z m"   .   kirigami-close-folds)     ; Close all folds
   ("C-c z <tab>" . kirigami-toggle-fold))    ; Toggle fold at point

  )

;;;; -------------------------
;;;; Org-mode Integration
;;;; -------------------------

;; (defun tuanany-org-enable-kirigami ()
;;   "Enable kirigami inside org src edit buffers."
;;   (when (derived-mode-p 'org-mode)
;;     (add-hook 'org-src-mode-hook #'kirigami-mode)))

;; (add-hook 'org-mode-hook #'tuanany-org-enable-kirigami)

;; ;;;; Optional: enable directly in org-src buffers

;; (add-hook 'org-src-mode-hook #'kirigami-mode)

(provide 'tuanany-helper-kirigami)
;;; tuanany-helper-kirigami.el ends here
