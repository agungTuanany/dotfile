;;;; tuanany-tools-org-mode.el --- org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: org-mode, note taking

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

;;;; Code:
;; https://stackoverflow.com/a/10091330/217812
(use-package org
  :defer
  :custom
  (org-agenda-include-diary t)
  ;; Where the org file live
  (org-directory "~/Documents/org-masters/")
  ;; Where archive should go | GTD Get Things Done
  (org-archive-location (concat (expand-file-name "~/Documents/org-masters/private/org-roam/gtd/archives.org") "::"))
  ;; Make sure we see syntax highlighting
  (org-src-fontify-natively t)
  ;; I don't use it for subs/super scripts
  (org-use-sub-superscripts nil)
  ;; Should everything be hidden?
  (org-startup-folded 'content)
  (org-M-RET-may-split-line '((default . nil)))
  ;; Don't hide stars
  (org-hide-leading-stars nil)
  (org-hide-emphasis-markers nil)
  ;; Show utf-8 chars
  (org-pretty-entities t)
  ;; Put timestamp when finished a todo
  (org-log-done 'time)
  ;; timestamp when reschedule
  (org-log-reschedule t)
  ;; Don't indent the stars
  (org-startup-indented nil)
  (org-list-allow-alphabetical t)
  (org-image-actual-width nil)
  ;; Save notes into log drawer
  (org-log-into-drawer t)
  ;;
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  ;;
  (org-fontify-quote-and-verse-blocks t)
  ;; See down arrow instead of "..." when we have subtrees
  ;; (org-ellipsis "⤵")
  ;; Catch invisible edit
  (org-catch-invisible-edits 'show-and-error)
  ;; Only useful for property searching only but can slow down search
  (org-hierarchical-todo-statistics nil)
  ;; Unchecked boxes will block switching the parent to DONE
  (org-enforce-todo-checkbox-dependencies t)
  ;; Don't allow TODO's to close without their dependencies done
  (org-enforce-todo-dependencies t)
  (org-track-ordered-property-with-tag t)
  ;; Where should notes go to? Dont even use them tho
  (org-default-notes-file (concat org-directory "notes.org"))
  ;; The right side of | indicates the DONE states
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELED(c!)" "DELEGATED(p!)")))
  ;; Needed to allow helm to compute all refile options in buffer
  (org-outline-path-complete-in-steps nil)
  (org-deadline-warning-days 2)
  (org-log-redeadline t)
  (org-log-reschedule t)
  ;; Repeat to previous todo state
  ;; If there was no todo state, then dont set a state
  (org-todo-repeat-to-state t)
  ;; Refile options
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '(("~/Documents/org/private/org-roam/gtd/gtd.org" :maxlevel . 3)
                        ("~/Documents/org/private/org-roam/gtd/someday.org" :level . 1)
                        ("~/Documents/org/private/org-roam/gtd/tickler.org" :maxlevel . 1)
                        ("~/Documents/org/private/org-roam/gtd/repeat.org" :maxlevel . 1)
                        ))
  ;; Lets customize which modules we load up
  (org-modules '(;; ol-eww
                 ;; Stuff I've enabled below
                 org-habit
                 ;; org-checklist
                 ))
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-display-inline-images))
  :custom-face
  (org-scheduled-previously ((t (:foreground "orange"))))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (sqlite . t)
     (python . t)
     (java . t)
     ;; (cpp . t)
     (C . t)
     (emacs-lisp . t)
     (shell . t)))
  ;; Save history throughout sessions
  (org-clock-persistence-insinuate)
  :bind
  (:map global-map
        ("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  ("C-c <up>" . org-priority-up)
  ("C-c <down>" . org-priority-down)
  ("C-c C-g C-r" . org-shifmetaright)
  )

;;; tuanany-tools-org-mode.el ends here
