;;; tuanany-core-ui.el --- early UI setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Created: 2026

;; Package-Requires: ((emacs "^25.1"))
;; Version: 0.1.0
;; Keywords: interface, core modules

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
;; interface for core packages before all emacs packages get loaded.

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
;; Note: the structure files, is matter. do not sort the lines alphabetically.

;;; Three categories to config in core-ui
;; display
;; faces
;; window

;;;; Display

;;; Frame appearance window appearance

;; Hide unnecessary UI elements.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Set frame appearance - opacity configuration
(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Configure fringe width
(add-to-list 'default-frame-alist '(left-fringe . 6))
(add-to-list 'default-frame-alist '(right-fringe . 6))

;; Prevent implied frame resizing
(setopt frame-inhibit-implied-resize t)


;;; Fonts

;; Source Code Pro, 15 pt.
(add-to-list 'default-frame-alist
         '(font . "Source Code Pro-15"))

(set-face-attribute 'default nil
            :family "Source Code Pro"
            :height 140)


;;; Line numbers

;; Globally enable relative line numbers.
(setopt
 display-line-numbers-type    'relative
 display-line-numbers-width   3
 display-line-numbers-widen   t)

(global-display-line-numbers-mode 1)

;; Disable line numbers in special-purpose modes.
(dolist (mode '(custom-mode-hook
                Info-mode-hook
                lisp-interaction-mode-hook
                ibuffer-mode-hook
                compilation-mode-hook
                dired-mode-hook
                eshell-mode-hook
                help-mode-hook
                magit-mode-hook
                markdown-mode-hook
                org-mode-hook
                shell-mode-hook
                term-mode-hook
                text-mode-hook
                helpful-mode-hook
                messages-buffer-mode-hook))
  (add-hook mode
            (lambda ()
              (display-line-numbers-mode -1))))


;;; Cursors

;; cursor-type
(setopt cursor-type 'hollow
        x-stretch-cursor t)

;; Disable cursor blinking
;; (blink-cursor-mode -1)


;;; Ui feedback

(setopt echo-keystrokes 0.1
        visible-bell nil
        truncate-string-ellipsis "...")

;; Highlight the current line globally
;; (global-hl-line-mode 1)

;; Keep the current line highlighted consistently.
(setopt hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)


;;;; Faces

;;; Cursor face

(set-face-attribute 'cursor nil
                    :background "light goldenrod"
                    :foreground "black")

;; Line-number face

(set-face-attribute 'line-number-current-line nil
                    :inherit 'default
                    :foreground "#CFC0C5"
                    :slant 'normal
                    :weight 'regular)

;; Programming comment
;; (set-face-attribute 'font-lock-comment-face nil
;;                     :foreground "dim gray"
;;                     :slant 'italic)


;;; Whitespace

;; Customize tab higlighting
(with-eval-after-load 'whitespace
  (set-face-attribute 'whitespace-tab nil
                      :foreground "#636363"))


;;; Web mode

;; This face is defined by web-mode when that package is loaded.
(with-eval-after-load 'web-mode
  (set-face-attribute 'web-mode-current-element-highglight-face 'nil
                      :foreground "#ffffff"
                      :underline "gold"))


;;;; Windows

;;; Window layout

;; Set thresholds for automatic window splitting.
(setopt
 split-width-threshold 170
 split-height-threshold 80)

;; Reverse slots for side windows
(setopt window-sides-slots '(3 0 3 1))


;;; Scrolling

(setopt scroll-margin 0
        scroll-step 1
        scroll-conservatively 100)


;;; Fringe and buffer boundaries

;; Keep fringe indicator visible at buffer boundaries.
(setopt indicate-buffer-boundaries 'left)

;;; Window display rules

;; Add display-buffer rule here when needed
;; Example:
(setopt display-buffer-alist
        '(( "\\*Help\\*" display-buffer-same-window)))


(provide 'tuanany-core-ui)

;;; tuanany-core-ui.el ends here
