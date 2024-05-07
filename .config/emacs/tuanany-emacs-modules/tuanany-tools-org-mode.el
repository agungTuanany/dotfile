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
  :defer t
  :custom
  (org-agenda-include-diary t)
  (org-directory "~/Documents/org-masters/")        ;; Where the org file live
  (org-archive-location (concat (expand-file-name "~/Documents/org-masters/private/org-roam/gtd/archives.org") "::"))       ;; Where archive should go | GTD Get Things Done
  (org-src-fontify-natively t)                      ;; Make sure we see syntax highlighting
  (org-use-sub-superscripts nil)                    ;; I don't use it for subs/super scripts
  (org-startup-folded 'content)                     ;; Should everything be hidden?
  (org-M-RET-may-split-line '((default . nil)))
  (org-hide-leading-stars nil)                      ;; Don't hide stars
  (org-hide-emphasis-markers nil)
  (org-pretty-entities t)                           ;; Show utf-8 chars
  (org-log-done 'time)                              ;; Put timestamp when finished a todo
  (org-log-reschedule t)                            ;; timestamp when reschedule
  (org-startup-indented nil)                        ;; Don't indent the stars
  (org-list-allow-alphabetical t)
  (org-image-actual-width nil)
  (org-log-into-drawer t)                           ;; Save notes into log drawer
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  ;; (org-ellipsis "⤵")                               ;; See down arrow instead of "..." when we have subtrees
  (org-catch-invisible-edits 'show-and-error)         ;; Catch invisible edit
  (org-hierarchical-todo-statistics nil)              ;; Only useful for property searching only but can slow down search
  (org-enforce-todo-checkbox-dependencies t)          ;; Unchecked boxes will block switching the parent to DONE
  (org-enforce-todo-dependencies t)                   ;; Don't allow TODO's to close without their dependencies done
  (org-track-ordered-property-with-tag t)
  (org-default-notes-file (concat org-directory "notes.org"))     ;; Where should notes go to? Dont even use them tho
  (org-todo-keywords                                  ;; The right side of | indicates the DONE states
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELED(c!)" "DELEGATED(p!)")))
  (org-outline-path-complete-in-steps nil)            ;; Needed to allow helm to compute all refile options in buffer
  (org-deadline-warning-days 2)
  (org-log-redeadline t)
  (org-log-reschedule t)
  (org-todo-repeat-to-state t)                        ;; Repeat to previous todo state If there was no todo state, then don't set a state

  (org-refile-use-outline-path 'file)                 ;; Refile options
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-targets '(("~/Documents/org/private/org-roam/gtd/gtd.org" :maxlevel . 3)
                        ("~/Documents/org/private/org-roam/gtd/someday.org" :level . 1)
                        ("~/Documents/org/private/org-roam/gtd/tickler.org" :maxlevel . 1)
                        ("~/Documents/org/private/org-roam/gtd/repeat.org" :maxlevel . 1)
                        ))
  ;; customize loaded module
  (org-modules '(;; ol-eww
                 ;; stuff that I enabled
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
