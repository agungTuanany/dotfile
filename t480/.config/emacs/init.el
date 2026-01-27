;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Copyright (C) 2021-2026 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((Emacs "25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: init file, key mapping, modular file, custom config.

;;;; Package-Requires: ((Emacs "29.1"))

;;;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; In summary,
;; [-] :custom is used for setting variables and faces associated with the
;; package, before package loaded, only for "defcustom" variables not all
;; variables; while
;; [-] :config is used for executing Emacs Lisp code to
;; configure or initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to
;; fully customize and configure a package to suit your needs.

;; Thanks to:
;;
;; - James Cherti - https://github.com/jamescherti/minimal-emacs.d
;; - Lars Tveito https://github.com/larstvei/dot-emacs

;;; Code:

;; Recommended to have this at the top
;; Source: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; Emacs 29 support for tree-sitter built in.

;; Add custom tree-sitter grammar path
(setq treesit-extra-load-path `(,(concat user-emacs-directory "etc/var/tree-sitter-dist/")
                                ,(concat user-emacs-directory "etc/tree-sitter")))

(use-package no-littering :ensure t)
;; (use-package quelpa :ensure t)
;; (use-package quelpa-use-package :ensure t)

;; (require 'shortdoc)
(use-package emacs
  :ensure nil
  :defer nil

  ;; Variables that should be set before any packages
  :init
  (setq-default
   indent-tabs-mode nil)

  ;; Function calls
  (prefer-coding-system   'utf-8)           ;; Function call

  ;;; PERFORMANCE & MISC
  (setq
   inhibit-compacting-font-caches   t       ;; Do not exhaust GC memory
   load-prefer-newer                t       ;; Load newest version file

   ;; Coding Systems
   coding-system-aliases  (list (cons 'UTF-8 'utf-8)))

  ;;; FILE-HANDLING & HISTORIC
  ;; Manage Backup, autosave, custom, backup.
  ;;
  ;; Put all the file systems on '~/.config/emacs/etc' directory.
  ;; Do not clutter 'user-emacs-directory (~/.config/emacs/),
  ;; all unnecessary or secondary file take in (/etc/) directory.
  (setq
   backup-inhibited               nil
   file-name-handler-alist        nil


   ;; Remember and restore the last place you visited in a file
   auto-save-list-file-name (expand-file-name "etc/auto-save-list/.save-" user-emacs-directory)
   tramp-auto-save-directory (expand-file-name "etc/tramp-autosave/.tramp-" user-emacs-directory)
   recentf-save-file        (locate-user-emacs-file "etc/recentf")
   save-place-file          (locate-user-emacs-file "etc/saveplaces")
   savehist-file            (locate-user-emacs-file "etc/savehist")
   custom-file              (locate-user-emacs-file "etc/custom.el")
   )

  ;; kill the buffer without asking a live process attached to it
   (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  :custom
  ;;; Display
  (display-line-numbers         t)          ;; globally enable line numbers
  (display-line-numbers-type    'relative)
  (display-line-numbers-width   3)
  (display-line-numbers-widen   t)

  ;;; Dired
  (dired-listing-switches          "-alhv --group-directories-first")
  (dired-free-space                nil)
  (dired-dwim-target               t)  ; Propose a target for intelligent moving/copying
  (dired-deletion-confirmer        'y-or-n-p)
  (dired-filter-verbose            nil)
  (dired-recursive-deletes         'top)
  (dired-recursive-copies          'always)
  (dired-vc-rename-file            t)
  (dired-create-destination-dirs   'ask)
  ;; Suppress Dired buffer kill prompt for deleted dirs
  (dired-clean-confirm-killing-deleted-buffers nil)

  ;; This is a higher-level predicate that wraps `dired-directory-changed-p'
  ;; with additional logic. This `dired-buffer-stale-p' predicate handles remote
  ;; files, wdired, unreadable dirs, and delegates to dired-directory-changed-p
  ;; for modification checks.
  (auto-revert-remote-files  nil)
  (dired-auto-revert-buffer  'dired-buffer-stale-p)

  ;; dired-omit-mode
  (dired-omit-verbose  nil)
  (dired-omit-files    (concat "\\`[.]\\'"))

  (ls-lisp-verbosity   nil)
  (ls-lisp-dirs-first  t)

   ;;; Ediff

  ;; Configure Ediff to use a single frame and split windows horizontally
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  ;;; ispell

  ;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
  ;; when no `ispell' dictionary is set.
  (text-mode-ispell-word-completion nil)
  (ispell-silently-savep            t)

  ;;; ibuffer

  (ibuffer-formats
   '((mark modified read-only locked
           " " (name 55 55 :left :elide)
           " " (size 8 -1 :right)
           " " (mode 18 18 :left :elide) " " filename-and-process)
     (mark " " (name 16 -1) " " filename)))

  ;;; abbrev
  (save-abbrevs 'silently)


  ;;; UI/UX
  (echo-keystrokes              0.05)       ;; the value is 0.25
  (visible-bell                 t)
  (truncate-string-ellipsis     "…")

  ;;; Paren Matching
  (show-paren-delay                     0.1)
  (show-paren-when-point-inside-paren   t)
  (show-paren-when-point-in-periphery   t)
  (show-paren-style 'expression)

  ;;; Input/Output
  (read-answer-short  t)                    ;; Allow for shorter responses
  (use-short-answers  t)                    ;; use 'y' or 'n'

  ;;; Evaluation
  (eval-expression-print-length nil)        ;; Disable truncation
  (eval-expression-print-level  nil)


  ;;; Frame/Window
  (default-frame-alist '((alpha-background . 90)
                         ;; (font . "Source Code Pro-12")
                         (fullscreen . maximize-window)))


  (fill-column                     80)
  (frame-inhibit-implied-resize    t)       ;; Fonts independent from any resized frame
  (split-width-threshold           170)     ;; Prefer Vertical splits
  (split-height-threshold          80)
  (window-sides-slots              '(3 0 3 1))



  ;;; Text/Editing
  (scroll-conservatively             100)
  (sentence-end-double-space         nil)           ;; End sentence with 1 space not 2
  (tab-width                         4)             ;; tabs are evil
  (narrow-to-defun-include-comments  t)
  (x-stretch-cursor                  t)             ;; Make cursor stretch to cover wider characters
  (x-underline-at-descent-line       t)             ;; position underline as descent line

  ;;; Navigation/Search
  (switch-to-buffer-in-dedicated-window   nil)
  (switch-to-buffer-obey-display-actions  t)        ;; Treat manual switching of buffers the same as programmatic
  (outline-blank-line                     t)        ;; Maintaining blank lines between folded section
  (search-invisible                       nil)      ;; Prevent Emacs from searching folded section

  ;;; Debugging
  (debugger-stack-frame-as-list           t)

  ;;; History
  (history-length                         1000)
  (history-delete-duplicates              t)

  ;;; File Handling
  (auto-save-default                      nil)
  (make-backup-files                      nil)
  (create-lockfiles                       nil)

  ;;; Buffer/Minibuffer
  (confirm-nonexistent-file-or-buffer   nil)
  (delete-by-moving-to-trash            not noninteractive)
  (enable-recursive-minibuffers         t)
  (inhibit-startup-echo-area-message    t)
  (initial-buffer-choice                t)          ;; open *scratch* buffer
  (kill-buffer-delete-auto-save-files   t)
  (kill-do-not-save-duplicates          t)
  (kill-ring-max                        30)
  (minibuffer-prompt-properties         '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
  (next-line-add-newlines               nil)        ;; Disable auto-adding a new line
  (uniquify-buffer-name-style           'forward)
  (use-dialog-box                       nil)

  ;;; Undo/redo
  (undo-limit         (* 13 160000))
  (undo-strong-limit  (* 13 240000))
  (undo-outer-limit   (* 13 24000000))

  ;;; Bookmars
  (bookmark-save-flag 1)                    ;; Save bookmarks immediately after change

  ;;; TRAMP
  (tramp-verbose                             1)
  (tramp-completion-reread-directory-timeout 50)
  (remote-file-name-inhibit-cache            50)

  ;;; Imenu
  (imenu-max-item-length 106)               ;; Prevent truncation of long function names

  ;;; Auto revert
  ;; Auto-revert in Emacs is a feature that automatically updates the contents of
  ;; a buffer to reflect changes made to the underlying file.
  (revert-without-query             (list ".")) ;; Do not prompt
  (auto-revert-stop-on-user-input   nil)
  (auto-revert-verbose              t)

  ;; `recentf' is an that maintains a list of recently accessed files.
  (recentf-max-saved-items   300) ; default is 20
  (recentf-max-menu-items    15)
  (recentf-auto-cleanup      'mode)
  (recentf-exclude           nil)

  ;;; hl-line-mode
  ;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
  ;; clutter and slightly improving `hl-line-mode' performance.
  (hl-line-sticky-flag          nil)
  (global-hl-line-sticky-flag   nil)

  ;;; icomplete
  ;; Do not delay displaying completion candidates in `fido-mode' or
  ;; `fido-vertical-mode'
  (icomplete-compute-delay 0.01)

  ;;; ispell
  ;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
  ;; when no `ispell' dictionary is set.
  (text-mode-ispell-word-completion   nil)
  (ispell-silently-savep              t)

  ;;; ibuffer
  (ibuffer-formats
      '((mark modified read-only locked
              " " (name 55 55 :left :elide)
              " " (size 8 -1 :right)
              " " (mode 18 18 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

  ;;; flyspell
(flyspell-issue-welcome-flag nil)

;; Improves flyspell performance by preventing messages from being displayed for
;; each word when checking the entire buffer.
(flyspell-issue-message-flag nil)

  ;; ================================================
  ;;; LOAD EARLY PACKAGES AND HOOKS
  ;; ================================================
  :hook
  (prog-mode       . display-line-numbers-mode)
  (prog-mode       . hs-minor-mode)
  (prog-mode       . outline-indent-minor-mode)
  (prog-mode-hook  . electric-indent-local-mode)
  (before-save     . tuanany--untabify-before-save)

   ;; After everything is loaded, configure modes and keybindings
  ;;
  :config
  ;; simply toggle of customizable variables
  (blink-cursor-mode       0)             ;; Blink gives me a sick eyes
  (electric-pair-mode      1)             ;; Automatic parens pairing
  (global-auto-revert-mode 1)             ;; Refresh the file if has changed
  (pending-delete-mode     1)             ;; Typed text replaces if the selection is active
  (recentf-mode            1)
  (save-place-mode         1)
  (savehist-mode           1)

  ;; UI tweaks
  (menu-bar-mode   -1)
  (menu-bar-mode   -1)
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)

  ;; Font/Font size
  ;; (when (display-graphic-p)
  ;;   ;; Clear any font scaling
  ;;   (setq face-font-rescale-alist nil)

  ;;   ;; Set the font directly
  ;;   (set-face-attribute 'default nil
  ;;                       :family "Source Code Pro"
  ;;                       :height 120) ;; 12pt = 120

  ;;   ;; Force immediate update
  ;;   (redraw-frame (selected-frame))

  ;;   ;; Set for new frames too
  ;;   (add-to-list 'default-frame-alist
  ;;                '(font . ,(font-xlfd-name
  ;;                           (font-spec :family "Source Code Pro" :height 120)))
  ;;                t)
  ;;   (message "Font set to Source Code Pro 12pt"))

  ;; Unset unwanted keys (e.g spurious touchscreen events)
  (global-unset-key [touchscreen-begin])
  (global-unset-key [touchscreen-end])
  (global-unset-key [touchscreen-update])

  ;; Keybindings
  (keymap-global-unset "C-x C-c"      'save-buffers-kill-terminal)
  (keymap-global-unset "C-x C-o"      'delete-blank-lines)
  (keymap-global-set   "C-c f"        'find-name-dired)
  (keymap-global-set   "C-x C-c C-c"  'save-buffers-kill-emacs)
  (keymap-global-set   "C-x C-o"      'other-window)
  (keymap-global-set   "C-x M-f"      'recentf-open-files)
  (keymap-global-set   "C-x C-b"      'ibuffer)
  ;; (keymap-global-set "C-x C-/" 'goto-last-change)

  (toggle-frame-maximized)                  ;; Always use all screen for emacs
  (set-fringe-mode 6)                       ;; Give some breating room for symbols

  ;; Disable line-numbers in specific mode
  (dolist (mode '(Custom-mode-hook
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
                  ;; package-menu-hook
                  ;; u-mode-hook
                  shell-mode-hook
                  term-mode-hook
                  text-mode-hook
                  treemacs-mode-hook
                  helpful-mode-hook
                  messages-buffer-mode-hook
                  dashboard-mode-hook))
    (add-hook mode(lambda () (display-line-numbers-mode -1))))

  (dolist (tuanany-module-path '("tuanany-emacs-modules" "tuanany-lisp"))
    (add-to-list 'load-path
                 (locate-user-emacs-file tuanany-module-path)))

  (defvar tuanany-addons '(
                           ;; ==== UI ====
                           "tuanany-ui-dashboard.el"
                           "tuanany-ui-doom-modeline.el"
                           "tuanany-ui-time.el"
                           "tuanany-ui.el"
                           ;; ==== COMPLETION ====
                           "tuanany-completion-cape.el"
                           "tuanany-completion-consult.el"
                           "tuanany-completion-corfu.el"
                           "tuanany-completion-custom.el"
                           "tuanany-completion-embark-consult.el"
                           "tuanany-completion-embark.el"
                           "tuanany-completion-marginalia.el"
                           "tuanany-completion-orderless.el"
                           "tuanany-completion-yasnippet.el"
                           "tuanany-completion-vertico.el"
                           ;; ==== HELPER ====
                           "tuanany-helper-dumb-jump.el"
                           "tuanany-helper-flycheck.el"
                           "tuanany-helper-goto-last-change.el"
                           ;; "tuanany-helper-helpful.el"
                           "tuanany-helper-highlight-indentation.el"
                           "tuanany-helper-iedit.el"
                           "tuanany-helper-ispell.el"
                           "tuanany-helper-minibuffer.el"
                           "tuanany-helper-outline-indent.el"
                           "tuanany-helper-pdf-tools.el"
                           "tuanany-helper-whitespace.el"
                           "tuanany-helper-multiple-cursors.el"
                           ;; "tuanany-helper-shrface.el"
                           ;; ==== TOOLS ====
                           ;; "tuanany-tools-evil.el"
                           "tuanany-tools-eww.el"
                           "tuanany-tools-magit.el"
                           "tuanany-tools-org-mode.el"
                           "tuanany-tools-proced.el"
                           "tuanany-tools-projectile.el"
                           "tuanany-tools-shell.el"
                           "tuanany-tools-skewer.el"
                           "tuanany-tools-tree-sitter.el"
                           ;; "tuanany-tools-browse-url.el"
                           ;; "tuanany-tools-impatient-mode.el"
                           ;; "tuanany-tools-prog-mode.el"
                           ;; ==== LANGUAGES ====
                           "tuanany-lang-clojure-mode.el"
                           "tuanany-lang-js2-mode.el"
                           "tuanany-lang-racket-mode.el"
                           "tuanany-lang-web-mode.el"
                           "tuanany-lang-yaml-mode.el"
                           "tuanany-lang-ruby-mode.el"
                           ))

  (dolist (x tuanany-addons)
    (load x))

  ;; Enable these
  (dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
    (put c 'disabled nil))

  ;; And disable these
  (dolist (c '(eshell project-eshell overwrite-mode iconify-frame diary))
    (put c 'disabled t))
  (put 'eshell 'disabled nil)

  ;; Load custom-file if it exists
  (when (file-exists-p custom-file)
    (load custom-file))

  (defun tuanany--untabify-before-save ()
    "Forcefully convert all tabs to spaces before saving, except Makefiles."
    (unless (or (derived-mode-p 'makefile-mode)
                (string-match-p "\\.mk$" buffer-file-name))
      (let ((inhibit-message t))  ; Suppress "Untabified N regions" message
        (untabify (point-min) (point-max))
        (setq indent-tabs-mode nil)  ; Ensure future indentation uses spaces
        (when (fboundp 'whitespace-cleanup)  ; Additional cleanup if available
          (whitespace-cleanup)))))

   (defun tuanany-toggle-fold ()
      "Toggle fold all lines larger than indentation on current line"
      (interactive)
      (let ((col 1))
        (save-excursion
          (back-to-indentation)
          (setq col (+ 1 (current-column)))
          (set-selective-display
           (if selective-display nil (or col 1))))))

  :custom-face
  (cursor ((t (:background "light goldenrod" :foreground "black"))))
  (web-mode-current-element-highlight-face ((t (:foreground "#ffffff" :underline "gold"))))
  (font-lock-comment-face ((t (:foreground "dim gray" :slant italic))))
  (line-number-current-line ((t (:inherit default :foreground "#CFC0C5" :slant normal :weight regular))))
  (whitespace-tab ((t (:foreground "#636363"))))
  ;; (marginalia-documentation ((t (:inherit nil :foreground "LemonChiffon4" :weight normal))))

  ) ;; END EMACS-PACKAGE

;;; init.el ends here
