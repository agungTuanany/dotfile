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
;;
;; In summary,
;; [-] :custom is used for setting variables and faces associated with
;; the package, before package loaded; while
;; [-] :config is used for executing Emacs Lisp code to configure or
;; initialize the package after it is loaded.
;; You can use both keywords together in a use-package declaration to
;; fully customize and configure a package to suit your needs.

(use-package org
  :defer t
  :hook (org-mode . tuanany--org-setup)
  :custom
  (org-directory "~/Documents/org-masters/")        ;; Where the org file live
  (org-archive-location (concat (expand-file-name "~/Documents/org-masters/private/org-roam/gtd/archives.org") "::"))   ;; Where archive should go | GTD Get Things Done
  (org-M-RET-may-split-line '((default . nil)))
  (org-catch-invisible-edits 'show-and-error)       ;; Catch invisible edit
  (org-deadline-warning-days 2)
  (org-default-notes-file (concat org-directory "tuanany-notes.org"))       ;; Where should notes go to? Dont even use them tho
  ;; (org-ellipsis "⤵")                                ;; See down arrow instead of "..." when we have subtrees
  (org-enforce-todo-checkbox-dependencies t)        ;; Unchecked boxes will block switching the parent to DONE
  (org-enforce-todo-dependencies t)                 ;; Don't allow TODO's to close without their dependencies done
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars nil)                      ;; Don't hide stars
  (org-hierarchical-todo-statistics nil)            ;; Only useful for property searching only but can slow down search
  (org-image-actual-width nil)
  (org-loop-over-headlines-in-active-region 'start-level)
  (org-list-allow-alphabetical t)
  (org-outline-path-complete-in-steps nil)          ;; Needed to allow helm to compute all refile options in buffer
  (org-pretty-entities t)                           ;; Show utf-8 chars
  (org-src-fontify-natively t)                      ;; Make sure we see syntax highlighting
  (org-startup-folded 'content)                     ;; Should everything be hidden?
  (org-startup-indented nil)                        ;; Don't indent the stars
  (org-startup-align-all-tables)                    ;;
  (org-startup-shrink-all-tables)                   ;;
  (org-todo-repeat-to-state t)                      ;; Repeat to previous todo state If there was no todo state, then don't set a state
  (org-track-ordered-property-with-tag t)
  (org-use-sub-superscripts nil)                    ;; I don't use it for subs/super scripts

  (org-insert-heading-respect-content t)
  (org-read-date-prefer-future 'time)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path 'file)               ;; Refile options
  (org-refile-targets '(("~/Documents/org-masters/private/org-roam/gtd/gtd.org" :maxlevel . 3)
                        ("~/Documents/org-masters/private/org-roam/gtd/someday.org" :level . 1)
                        ("~/Documents/org-masters/private/org-roam/gtd/tickler.org" :maxlevel . 1)
                        ("~/Documents/org-masters/private/org-roam/gtd/repeat.org" :maxlevel . 1)
                        (org-agenda-files . (:max-levels . 2))
                        (nil . (:max-level . 2))
                        ))

  (org-modules '(                                   ;; customize loaded module
                 org-habit
                 ;; ol-eww
                 ;; org-checklist
                 ))

  ;; ORG-LOG
  (org-log-done 'time)                              ;; Put timestamp when finished a todo
  (org-log-into-drawer t)                           ;; Save notes into log drawer
  (org-log-note-clock-out nil)
  (org-log-redeadline t)
  (org-log-reschedule t)                            ;; timestamp when reschedule

  ;; === ORG-TODO ===
  (org-todo-repeat-to-state t)                      ;; Repeat to previous todo state If there was no todo state, then don't set a state
  (org-todo-keywords
   '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" "CANCELED(c!)" "WAITING(w!)" "DELEGATED")
     ))

  ;; TODO colors
  (org-todo-keyword-faces
   '(
     ("TODO" . (:foreground "GoldenRod" :weight bold))
     ("PLANNING" . (:foreground "DeepPink" :weight bold))
     ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
     ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
     ("BLOCKED" . (:foreground "Red" :weight bold))
     ("DONE" . (:foreground "LimeGreen" :weight bold))
     ("OBE" . (:foreground "LimeGreen" :weight bold))
     ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
     ("CANCELED" . (:foreground "DarkRed" :weight bold))
     ("DELEGATED" . (:foreground "DarkOrange" :weight bold))
     ))
  (org-capture-templates
   '(
     ("j" "Work Log Entry"
      entry (file+datetree "~/Documents/org-masters/tuanany-work-log.org")
      "* [ ] %?"
      :empty-lines 0)

     ("n" "Simple Note"
      entry (file+headline "~/Documents/org-masters/tuanany-work-log.org" "Random Notes")
      "** [ ] %?"
      :empty-lines 0)

     ("d" "Door Codes"
      entry (file+headline "~/Documents/org-masters/tuanany-work-log.org" "Door Codes")
      "** [ ] %?"
      :empty-lines 0)

     ("g" "General TO-DO"
      entry (file+headline "~/Documents/org-masters/tuanany-todo.org" "General Task")
      "* TODO [#B] %?\n:Created: %T\n "
      :empty-lines 0)

     ("c" "Code To-Do"
      entry (file+headline "~/Documents/org-masters/tuanany-todo.org" "Code Related Tasks")
      "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
      :empty-lines 0)

     ("m" "Meeting"
      entry (file+datetree "~/Documents/org-masters/tuanany-meetings.org")
      "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
      :tree-type week
      :clock-in t
      :clock-resume t
      :empty-lines 0)
     ))
  (org-tag-alist '(
                   ;; Ticket types
                   (:startgroup . nil)
                   ("@bug" . ?b)
                   ("@feature" . ?u)
                   ("@spike" . ?j)
                   (:endgroup . nil)

                   ;; Ticket flags
                   ("@write_future_ticket" . ?w)
                   ("@emergency" . ?e)
                   ("@research" ?r)

                   ;; Meeting types
                   (:startgroup . nil)
                   ("big_sprint_review" . ?i)
                   ("cents_sprint_retro" . ?n)
                   ("dsu" . ?d) ;; domain specific update
                   ("grooming" . ?g)
                   ("sprint_retro" ?s)
                   (:endgroup . nil)

                   ;; Code TODOs tags
                   ("QA" . ?q)
                   ("backend" . ?k)
                   ("broken_code" . ?c)
                   ("frontend" . ?f)

                   ;; Special tags
                   ("CRITICAL" . ?x)
                   ("obstacle" . ?o)

                   ;; Meeting tags
                   ("HR" . ?h)
                   ("general" . ?l)
                   ("meeting" . ?m)
                   ("misc" . ?z)
                   ("planning" . ?p)

                   ;; Work Log Tags
                   ("accomplishment" . ?a)
                   ))
  ;; Tag colors
  (org-tag-faces
   '(
     ("planning"  . (:foreground "mediumPurple1" :weight bold))
     ("backend"   . (:foreground "royalblue1"    :weight bold))
     ("frontend"  . (:foreground "forest green"  :weight bold))
     ("QA"        . (:foreground "sienna"        :weight bold))
     ("meeting"   . (:foreground "yellow1"       :weight bold))
     ("CRITICAL"  . (:foreground "red1"          :weight bold))
     )
   )

  ;; ORG-AGENDA
  (org-agenda-files '("~/Documents/org-masters/org-agenda/"))
  (org-agenda-include-diary t)
  (org-agenda-skip-deadline-if-done t)

  ;; https://github.com/james-stoup/emacs-org-mode-tutorial?tab=readme-ov-file#orgd080503
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, ?C."
    (let ((subtree-end (save-excursion (org-end-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (org-agenda-custom-commands
   '(
     ;; Daily Agenda & TODOs
     ("d" "Daily agenda and all TODOs"
      ;; Display items with priority A
      ((tags "PRIORITY=\"A\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-overriding-header "High-priority unfinished tasks:")))
       ;; View 7 days in the calendar view
       (agenda "" ((org-agenda-span 7)))
       ;; Display items with priority B (really it is view all items minus A & C)
       (alltodo ""
                ((org-agenda-skip-function '(or (air-drop-skip-subtree-if-priority ?A)
                                                (air-drop-skip-subtree-if-priority ?C)
                                                (org-agenda-skip-if nil '(schedule deadline))))
                 (org-agenda-overriding-header "ALL normal priority tasks: ")))
       ;; Display items with priority C
       (tags "PRIORITY=\"C\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
       ;; Don't compress things (change to suite your taste)
       (org-agenda-compact-blocks nil)
       ))
     ))
  :custom-face
  (org-block ((t (:foreground "unspecified" :inherit fixed-pitch))))
  (org-checkbox ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit (shadow fixed-pitch)))))
  (org-document-info ((t (:foreground "dark orange"))))
  (org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  (org-indent ((t (:inherit (org-hide fixed-pitch)))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-property-values ((t (:inherit fixed-pitch))))
  (org-scheduled-previously ((t (:foreground "orange"))))
  (org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-table ((t (:inherit fixed-pitch :foreground "#83A598"))))
  (org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  (org-verbatim ((t (:inherit (shadow fixed-pitch)))))

  ;; Related to org paragraph fonts
  (variable-pitch ((t (:family "Source Code Pro" :height 1.0 :weight medium))))
  (fixed-pitch ((t (:family "Source Code Pro" :height: 1.0 :weight medium))))
  ;; (org-default ((t (:family "Source Code Pro" :height 1.15 :weight medium :foreground "SpringGreen4"))))
  :bind
  ("C-c <up>" . org-priority-up)
  ("C-c <down>" . org-priority-down)
  (:map global-map
        ("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture)
        ("C-c e" . tuanany--toggle-org-emphasis-markers))

  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))   ;; Associate all org files with org-mode
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

  (dolist (tuanany--org-level-faces '((org-level-1 . 1.20)
                                      (org-level-2 . 1.15)
                                      (org-level-3 . 1.13)
                                      (org-level-4 . 1.1)
                                      (org-level-5 . 1.1)
                                      (org-level-6 . 1.1)
                                      (org-level-7 . 1.1)
                                      (org-level-8 . 1.1)))

    (set-face-attribute (car tuanany--org-level-faces) nil :family "Source Code Pro" :weight 'semibold :height (cdr tuanany--org-level-faces)))
  )

(defun tuanany--org-setup ()
  "Local function to setup all `org-mode-hook in one place."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (org-display-inline-images))

(defun tuanany--toggle-org-emphasis-markers ()
  "Toggle the visibility of Org-mode emphasis markers."
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  ;; Refresh the current buffer to apply the changes
  ;; (org-toggle-pretty-entities)
  (font-lock-flush)
  (font-lock-ensure))

(use-package org-super-agenda
  :after org)

(use-package comment-tags)
;; (require 'org-indent)
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-pomodoro
  :custom
  (org-pomodoro-length 20)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 20))

(require 'ox-latex)
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-packages-alist '(("" "amsmath" t)))


;;; tuanany-tools-org-mode.el ends here
