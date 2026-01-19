;;;; tuanany-helper-dumb-jump.el --- dumb jump -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2026
;; Version: 0.1.0
;; Keywords: jump, last change

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
;; A basic 'go to' functionalit that work really well. No need for LSP any more.

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

;; (use-package goto-last-change
;;   :ensure t
;;   :defer t

;;   :bind
;;   (("C-x C-/" 'goto-last-change)
;;    ("C-x /" 'goto-last-change)))

(use-package goto-last-change
  :ensure t
  :bind (("C-x C-/" . goto-last-change)
         ("C-x /"   . goto-last-change)))  ; Alternative binding
;;; tuanany-helper-dumb-jump.el ends here
