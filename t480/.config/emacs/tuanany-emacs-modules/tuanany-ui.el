;;;; tuanany-ui.el --- UI -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: global ui,

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

(use-package doom-themes
  :config
  (setq-default doom-theme-enable-bold t
                doom-theme-enable-italic t)
  (load-theme 'doom-monokai-octagon t)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :hook
  ((prog-mode . which-function-mode)
   (after-init . which-key-mode))
  :custom
  (which-key-idle-delay 0.5)
  )

(use-package paredit
  :hook
  ((racket-mode . paredit-mode)
   (racket-repl-mode . paredit-mode))
  (clojure-mode . paredit-mode))

;; (face-attribute 'default :height)
;; (set-face-attribute 'default nil :height 100)  ;; 140 = 14pt
(set-face-attribute 'default nil :font "Source Code Pro" :height 120)


;;; tuanany-ui.el ends here
