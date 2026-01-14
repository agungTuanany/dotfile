;;;; tuanany-tools-browse-url.el --- browse url -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: browser

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

;; (use-package browse-url
;;   :ensure nil
;;   :custom
;;   ;; Emacs can't find browser binaries
;;   ;; (browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
;;   (browse-url-firefox-program "/usr/bin/firefox-developer-edition")
;;   ;; Neat trick to open that route to different places
;;   (browse-url-firefox-new-window-is-tab t)
;;   :config
;;   (put 'browse-url-handlers 'safe-local-variable (lambda (x) t)))

;;; tuanany-tools-browse-url.el ends here
