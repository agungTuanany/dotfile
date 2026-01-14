;;;; tuanany-completion-consult-.el --- completion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

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

;;; Code:

(use-package consult
  :ensure
  ;; :after projectile
  :bind (("C-s" . consult-line)
         ("C-c M-x" . consult-mode-command)
         ("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("C-z" . consult-theme)
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)
         ;; :map projectile-command-map
         ;; ("b" . consult-project-buffer)
         :map prog-mode-map
         ("M-g o" . consult-imenu))

  :init
  (defun remove-items (x y)
    (setq y (cl-remove-if (lambda (item) (memq item x)) y))
    y)

  ;; Any themes that are incomplete/lacking don't work with centaur tabs/solair mode
  (setq tuanay-themes-blacklisted '(
                                   ;; doom-tomorrow-night
                                   ayu-dark
                                   ayu-light
                                   doom-acario-dark
                                   doom-acario-light
                                   doom-homage-black
                                   doom-lantern
                                   doom-manegarm
                                   doom-meltbus
                                   doom-rougue
                                   light-blue
                                   manoj-black
                                   tao
                                   ))
  (setq consult-themes (remove-items tuanay-themes-blacklisted (custom-available-themes)))
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-narrow-key "<")
  (setq consult-line-start-from-top nil)

  (defun tuanany-consult-line (&optional arg)
    "Start consult search with selected region if any.
If used with a prefix, it will search all buffers as well."
    (interactive "p")
    (let ((cmd (if current-prefix-arg '(lambda (arg) (consult-line-multi t arg)) 'consult-line)))
      (if (use-region-p)
          (let ((regionp (buffer-substring-no-properties (region-beginning) (region-end))))
            (deactivate-mark)
            (funcall cmd regionp))
        (funcall cmd ""))))

  :config
  ;; Better integration with embark
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

  ;; Improved consult buffer sorting
  ;; (setq consult-buffer-sources
  ;;       '(consult--source-hidden-buffer
  ;;         consult--source-buffer
  ;;         consult--source-recent-file
  ;;         consult--source-project-buffer
  ;;         consult--source-bookmark
  ;;         consult--source-project-buffer
  ;;         consult--source-project-recent-file))
  )

(use-package consult-ag
  :ensure
  ;; :bind (:map projectile-command-map
  ;;             ("s s" . consult-ag)
  ;;             ("s g" . consult-grep))
  :config
  (setq consult-ag-args "ag --nocolor --nogroup --numbers")
  )

(use-package consult-org-roam
  :ensure t
  :after (org-roam consult)
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ag)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers nil)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  )

(with-eval-after-load 'embark
  (with-eval-after-load 'consult
    (require 'embark-consult)
    (embark-consult-setup)))
;;; tuanany-completion-consult.el ends here
