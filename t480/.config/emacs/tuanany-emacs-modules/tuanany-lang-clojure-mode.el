;;;; tuanany-lang-clojure-mode.el --- Programming Languages Specified-*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1") (clojure-mode "5.13") (cider "1.6") (pareidt "26"))
;; Created: 2025
;; Version: 0.2.0
;; Keywords: clojure, programming language

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

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.edn\\'")
  :hook
  (clojure-mode . subword-mode)
  (subword-mode . paredit-mode)
  )

(use-package cider
  :bind-keymap
  ("C-c u" . cider-user-ns)
  :hook (clojure-mode . tuanany-auto-cider-jack-in)
  :config
  (setq nrepl-log-message                     t
        cider-auto-select-error-buffer        t
        cider-doc-auto-select-buffer          t
        cider-repl-display-help-banner        nil
        cider-repl-history-file               "~/.config/emacs/etc/cider-history"
        cider-repl-pop-to-buffer-on-connect   t
        cider-repl-result-prefix              ";; =>"
        cider-repl-use-clojure-font-lock      t
        cider-repl-wrap-history               t
        cider-show-error-buffer               t
        cider-use-overlays                    nil
        )

  (defun tuanany-auto-cider-jack-in ()
    "Automatically start CIDER when opening the first .clj .cljs file"
    (unless (cider-connected-p)
      (message "Starting CIDER REPL…")
      (cider-jack-in nil)))

  (defun tuanany-cider-print-eval-result-in-repl (result)
    "Insert RESULT into the *cider-repl* buffer, like native REPL output."
    (when-let ((repl (cider-current-repl 'clj)))
      (with-current-buffer repl
        (goto-char (point-max))
        (insert (format "\n;; => %s\n" result))
        (goto-char (point-max)))))

  ;; Advice: accept any number of args, pick out RESULT
  (advice-add 'cider--display-interactive-eval-result :after
              (lambda (&rest args)
                (let ((value (car args)))
                  (tuanany-cider-print-eval-result-in-repl value))))

  )




(use-package flycheck-clj-kondo
  :ensure t
  :after clojure-mode)

;; (provide 'tuanany-lang-clojure-mode)
;;; tuanany-lang-clojure-mode.el ends here
