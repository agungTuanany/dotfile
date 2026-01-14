;;;; tuanany-completion-yasnippet.el --- Programming Languages Specified-*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2025
;; Version: 0.1.0
;; Keywords: completion, snippets, template
;; Description: Snippet completion for Emacs, with per-mode snippet and Corfu -
;; integration
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
  :commands (yas-minor-mode yas-reload-all)
  :hook
  ((prog-mode text-mode conf-mode org-mode) . yas-minor-mode)
  :init

  :config
  ;; Explicitly set snippet directory before yas loaded
  ;; Only use your etc/yasnippet directory
  (setq yas-snippet-dirs
        (list (expand-file-name "etc/yasnippet/snippets" user-emacs-directory)))

  ;; Debug: Print whether directory exists
  (unless (file-directory-p (car yas-snippet-dirs))
    (message "⚠️  Snippet directory not found: %s" (car yas-snippet-dirs)))

  ;; Load manually and reload tables
  (when (file-directory-p (car yas-snippet-dirs))
    (yas-load-directory (car yas-snippet-dirs))
    (yas-reload-all)
    (message "✅ Yasnippet loaded snippets from %s)" (car yas-snippet-dirs))

  ;; Don't touch ~/.config/emacs/snippets
  (setq yas-prompt-function '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt))

  ;; function to manually reload snippets (useful after adding new ones)
  (defun tuanany--yas-reload()
    "Reload all yasnippet snippets and re-enable yasnippet."
    (interactive)
    (yas-reload-all)
    (message "Okay -- Yasnippet reloaded successfully."))

  ;; Automatically reload snippets when `org-mode` is reloaded
  (defun tuanany--yas-reload-after-org ()
    (when (derived-mode-p 'or-mode)
      (yas-reload-all)))
  (add-hook 'org-mode-hook #'tuanany--yas-reload-after-org)))

;; Optional integration with Corfu and Embark
(use-package yasnippet-capf
  :after (yasnippet cape)
  :init
  ;; Add yasnippet to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'tuanany-completion-yasnippet)
;;; tuanany-completion-yasnippet.el ends here
