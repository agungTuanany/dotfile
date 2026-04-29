;;; tuanany-helper-ispell.el --- Spell checking configuration -*- lexical-binding: t; -*-
;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: multiple occurrences in the same way simultaneously

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
;; Try to check the reference below, it's a great to start.
;; Ref: - http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs/
;;      - https://github.com/redguardtoo/emacs.d
;;      - https://github.com/gopar/.emacs.d?tab=readme-ov-file#spelling

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

;;;; -------------------------
;;;; Ispell / Hunspell Setup
;;;; -------------------------
(use-package ispell
  :ensure nil
  :init
  (let ((dict (expand-file-name "etc/.hunspell_en_US"
                                user-emacs-directory)))
    ;; ensure file exists with proper header
    (unless (file-exists-p dict)
      (with-temp-file dict
        (insert "0\n")))

    (setopt
     ispell-program-name "hunspell"
     ispell-dictionary "en_US"
     ispell-personal-dictionary dict
     ispell-silently-savep t
     ispell-extra-args (list "-d" "en_US" "-p" dict))))


(use-package flyspell
  :ensure nil
  :defer
  :hook ((prog-mode  . flyspell-prog-mode)
         (org-mode   . flyspell-mode)
         (text-mode  . flyspell-mode)
         (flyspell-mode . tuanany-flyspell-face-setup)
)
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-," . flyspell-goto-next-error)
              ("C-." . flyspell-auto-correct-word)))

(defun tuanany-flyspell-face-setup ()
  (set-face-attribute 'flyspell-incorrect nil
                      :underline '(:style wave :color "Red1"))
  (set-face-attribute 'flyspell-duplicate nil
                      :underline '(:style wave :color "DarkOrange")))





;;; tuanany-helper-ispell.el ends here
