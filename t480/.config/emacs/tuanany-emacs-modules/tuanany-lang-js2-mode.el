;;;; tuanany-lang-js2-mode.el --- js2-mode Programming Languages Specified-*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: javascript, programming language

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
;; js2-mode advantages:
;; - Syntax Checking
;; - Code Navigation
;; - Code Highlighting
;; - Code Folding
;; - ECMAScript Compatibility

;; https://stackoverflow.com/a/10091330/217812
;;
;; In summary,
;; [-] :custom is used for setting variables and faces associated with
;; the package, before package loaded; while
;; [-] :config is used for executing Emacs Lisp code to configure or
;; initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to
;; fully customize and configure a package to suit your needs.

;; Provides JavaScript development environment using `js2-mode`
;; with evaluation via `node` through `js-comint`.
;; Skewer integration is disabled in favor of Node.js workflow.
;;;; Code:

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'"     . js2-mode)           ;; open .js files in js2-mode
  :interpreter ("node"  . js2-mode)           ;; use node when interpreting
  :config
  (setq js2-highlight-level 3)
    (defun tuanany-js2-mode-keys()
    (keymap-set js2-mode-map "C-x C-e"  #'js-comint-send-last-sexp)
    (keymap-set js2-mode-map "C-M-x"    #'js-comint-send-last-sexp-and-go)
    (keymap-set js2-mode-map "C-c C-r"  #'js-comint-send-region)
    (keymap-set js2-mode-map "C-c C-b"  #'js-comint-send-buffer)
    (keymap-set js2-mode-map "C-c C-l"  #'js-comint-send-buffer-and-go)
    (keymap-set js2-mode-map "C-c C-c"  #'tuanany-run-js-file))

    (add-hook 'js2-mode-hook 'tuanany-js2-mode-keys))

(use-package js-comint
  :after js2-mode
  :commands (run-js)
  :config
  (setq js-comint-program-command "node")  ;; path to Node.js (/usr/bin/node)


  (defun tuanany-run-js-file ()
    "Run the current .js file with NodeJS."
    (interactive)
    (when (buffer-file-name)
      (compile (concat "node " (shell-quote-argument (buffer-file-name))))))
  )

;; Disable skewer from auto-loading in js2-mode
(with-eval-after-load 'skewer-mode
  (remove-hook 'js2-mode-hook #'skewer-mode)
  (remove-hook 'js-mode-hook #'skewer-mode))

;;; tuanany-lang-js2-mode.el ends here
