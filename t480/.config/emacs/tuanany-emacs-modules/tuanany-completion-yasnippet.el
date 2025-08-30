;;;; tuanany-completion-yasnippet.el --- Programming Languages Specified-*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2025
;; Version: 0.1.0
;; Keywords: completion, snippets, template
;; Description: Snippet completion for Emacs
;; Keywords: convenience, snippets, template

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

;; This config enables yasnippet, loads your custom snippets,
;; and integrates the yasnippet-snippets collection.

;;;; Code:
(use-package yasnippet
  :ensure t
  :config
  ;; Custom snippets dir: ~/.config/emacs/etc/yasnippet/snippets/
  (setq yas-snippet-dirs (list (expand-file-name "etc/yasnippet/snippets"
        user-emacs-directory)))
  ;; Enable globally
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  ;; Add package-provided snippets directory
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets"
               (file-name-directory (locate-library "yasnippet-snippets"))))
  ;; Reload all snippets (custom + package)
  (yas-reload-all))

;;; tuanany-completion-yasnippet.el ends here
