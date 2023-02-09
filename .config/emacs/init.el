;;; init.el --- emacs  -*- lexical-binding: t; outline-regexp: ";;;"; eval: (local-set-key (kbd "C-c i") #'consult-outline) -*-
;; Copyright (C) 2023
;; Author: ;;; Package -- init.el <agung.tuanany@gmaildotcom>
;; Keywords:
;;
;;; Commentary:
;; use systemlinks from the repo:
;; ln -s ~/Repo/agung_dotfile/.config/emacs ~/config/
;;
;;; Code:
;;; Presetup package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Presetup
(setq inhibit-startup-message t
      inhibit-compacting-font-caches t
      visible-bell t
      echo-keystrokes 0.5)

(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable toolbar
(tooltip-mode -1)          ; Disable tooltip
(menu-bar-mode -1)         ; Disable menu on top
(set-fringe-mode 10)       ; Give some breathing room
(electric-pair-mode 1)     ; auto-closing brackets
(auto-revert-mode 1)       ; refresh the file if has changed

(add-to-list 'default-frame-alist '(fullscreen, maximized))

;; line-number config
(column-number-mode)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
;; fix line-number-mode when encountering an overly long line
;; from showing double question marks
(setq line-number-display-limit-width 10000)

;; Memory Management
(setq gc-cons-threshold 5000000)

;; disable legacy algorithms 3DES
(setq-default gnutls-min-prime-bits 2048)
(setq-default gnutls-algorithm-priority "SECURE128")

;; Scracth buffer
(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)

;; initial buffer, let's display notes file instead as daily reminder
(setq-default remember-notes-bury-on-kill nil)
(setq-default remember-notes-initial-major-mode 'org-mode)
(setq initial-buffer-choice 'remember-notes)
;; There is a bit of mismatch between the keybindings 'remember-notes-mode' and 'org-mode'
(with-eval-after-load 'remember
  (define-key remember-notes-mode-map (kbd "C-c C-c") nil))

;; disable line-numbers for some modes
(dolist (mode '(Custom-mode-hook
                Info-mode-hook
                compilation-mode-hook
                dired-mode-hook
                eshell-mode-hook
                help-mode-hook
                magit-mode-hook
                markdown-mode-hook
                org-mode-hook
                package-menu-mode-hook
                shell-mode-hook
                term-mode-hook
                text-mode-hook
                treemacs-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

(custom-set-faces
 ;; disable background in line-number-current-line
 '(line-number-current-line ((t (:inherit default :foreground "#CFC0C5" :slant normal :weight bold))))
 ;; increase comment highlight, default one is lightly
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic :weight semibold :height 0.85))))
 '(show-paren-match ((t (:underline t :weight ultra-bold)))))

;; indentation config
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; trailing whitespace
(global-whitespace-mode)
(setq-default whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))

(setq-default whitespace-display-mappings
              '((tab-mark 9 [124 9] [92 9])))

(setq-default fill-column 80)

;;(set-face-attribute 'default nil :font "Liberation Mono" :height 90)
(set-face-attribute 'default nil :font "Source Code Pro" :height 90)

;; keybinding config
;; Make <ESC> quit prompt either to <C-g>
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; remember recent opened files
(recentf-mode 1)

;; remember minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
(setq-default global-auto-revert-non-file-buffers t)

;; set firefox as my default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox-developer-edition")

;; shorten Yes/No prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable blinking cursor
(setq blink-cursor-mode nil)

;; TODO: enable hs-minor-mode for toggle or use something else

;; make recenter with "C-l" start from top
(setq recenter-positions '(top middle bottom))

;; 'ediff'. when using ediff prepare horizontal split
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-split-window-function 'split-window-horizontally)

;; 'dired' prefer using one buffer unless another one is explicitly created
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; fix scrolling
(setq scroll-conservatively 10000
      scroll-preserve-screen-position t)

;; stop pasting at the mouse click point
;; middle-clicking is nice to paste, however it should not adjust point and
;; paste at the end then adjusted point
(setq mouse-yank-at-point t)

;; Display fringe indicators and fix line movement in 'visual-line-mode~
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; I don't like the remappings done to operate on visual lines (for
;; 'C-a', 'C-e' and 'C-k'), so I'm just undefining them.
(setcdr visual-line-mode-map nil)

;; enable every deactive command
;; avoiding confusion for beginners, with many option to choose.
(setq disabled-command-function nil)

;; Save clipboard data of other programs in the kill ring when possible
(setq save-interprogram-paste-before-kill t)

;; don't print integers in multiple format
(fset 'eval-expression-print-format 'ignore)

;; print backtraces in a lispier form
(setq debugger-stack-frame-as-list t)

;; display raw bytes as hex
(setq display-raw-bytes-as-hex t)

;; disable signal handler insanity
(setq attempt-stack-overflow-recovery nil)
(setq attempt-orderly-shutdown-on-fatal-signal nil)

;; unconditionally kill subprocess at exit
(setq confirm-kill-processes nil)

;; sentence spacing
(setq sentence-end-double-space nil)

;;========================================
;; Manage Backup, autosave, custom, backup

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq auto-save-list-file-prefix "~/.config/emacs/autosave/")
(setq auto-save-file-name-transforms '((".*"  "~/.config/emacs/autosave/" t)))

;; Backup files are created on save in the same directory as the file and
;; end in '~'.  They can be numbered which makes most sense combined with
;; a different save location and automatic pruning.
(setq backup-directory-alist
      '((".*" . "~/.config/emacs/backups")))
(setq version-control t)
(setq delete-old-versions t)

;; Lock files are put in the same directory to detect multiple users
;; trying to edit the same file. Given that I'm on a single-user system,
;; I can migrate them elsewhere as of Emacs 28.1.
(setq lock-file-name-transforms '((".*" "~/.config/emacs/lock/" t)))

(setq-default bookmark-default-file "~/.config/emacs/etc/bookmarks")

;; remember and restore the last place you visited in a file
(save-place-mode 1)
(setq-default save-place-file "~/.config/emacs/etc/saveplace")

;; move recentf to etc for not cluttering base dir
(setq-default recentf-save-file "~/.config/emacs/etc/recentf"
              recentf-max-saved-items 25
              recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; TRAMP https://www.gnu.org/software/tramp/
;; makes backup files, they should better be kept locally than remote
(setq-default tramp-backup-directory-alist backup-directory-alist)
(with-eval-after-load 'tramp-cache
  (setq-default tramp-persistency-file-name "~/.config/emacs/etc/tramp"))

;;; Plugins
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch))
(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; NOTE: make sure to configure a Github token before using this package
(use-package forge
  :ensure t
  :after magit)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq-default doom-theme-enable-bold t
                doom-theme-enable-italic t)
  (load-theme 'doom-monokai-classic t))

;; config modeline. The bar information on bottom
(use-package doom-modeline
  :ensure t
  :custom(
          (doom-modeline-height 15))
  :init (doom-modeline-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package treemacs
  :ensure t)

(use-package winum
  :ensure t
  :config
  (global-set-key (kbd "M-0") 'treemacs-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (winum-mode))

(use-package pdf-tools
  :pin manual
  :config
  ;;(pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1))))

(use-package eros
  :ensure t
  :init
  (eros-mode t)
  :config
  (setq eros-eval-result-prefix "⟹ "))

;;; Evil
(use-package evil
  :ensure t
  :init
  ;; going for emacs state whenever it makes sense
  ;; respect and use emacs motion state (emacs movement)
  ;; (setq evil-default-state 'emacs
  ;;       evil-emacs-state-modes nil
  ;;       evil-insert-state-modes nil
  ;;       evil-motion-state-modes nil
  ;;       evil-normal-state-modes '(text-mode prog-mode fundamental-mode
  ;;                                           css-mode conf-mode TeX-mode
  ;;                                           diff-mode))
  ;; (add-hook 'org-capture-mode-hook 'evil-insert-state)
  ;; (add-hook 'with-editor-mode-hook 'evil-insert-state)
  ;; (add-hook 'view-mode-hook 'evil-emacs-state)

  ;; base behavior adjustment
  (setq evil-cross-lines t
        evil-ex-substitute-global t   ;regex use 'g' at all matched
        evil-kill-on-visual-paste nil
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-symbol-word-search t
        evil-undo-system 'undo-redo
        evil-want-C-i-jump t
        evil-want-C-u-scroll t
        evil-want-fine-undo t
        evil-want-integration t
        evil-want-keybinding nil)

  ;; enable 'C-w' in emacs-state
  (with-eval-after-load 'evil-vars
    (setq evil-want-C-w-in-emacs-state nil))

  ;; (with-eval-after-load 'evil-common
  ;;   (evil-declare-motion 'recenter-top-bottom))

  ;; 'Y' as yank to the end of the line
  (setq evil-want-Y-yank-to-eol t)

  ;; disable setting up anyting in 'insert state keymap'
  (setq evil-disable-insert-state-bindings t)

  :config
  ;; keybindings
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "v") 'evil-visual-block)
  (define-key evil-normal-state-map (kbd "C-v") 'evil-visual-char)
  (define-key evil-visual-state-map (kbd "v") 'evil-visual-block)
  (define-key evil-visual-state-map (kbd "C-v") 'evil-visual-char)

  ;; use visual line motion even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
;;; completion config
;;company completion - for code/text completion
(use-package company
  :ensure t
  ;; :after lsp-mode
  :hook ((emacs-lisp-mode . (lambda ()
                              (setq-local company-backends '(company-elisp))))
         (emacs-lisp-mode . company-mode))
  :init
  (company-mode 1)
  :config
  ;; double dashes implies internal function, Cause 'winum' package use 'M-' with a number.
  (company-keymap--unbind-quick-access company-active-map)
  (company-tng-configure-default)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Vertico + Marginalia + Orderless
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :config
  (custom-set-faces
   '(marginalia-documentation ((t (:inherit nil :foreground "LemonChiffon4" :weight normal))))))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  )

;; use  flycheck instead flymake
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;;; Org-mode
(defun my-org-font-setup ()
  "Replace list hyphen with dot."
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'semibold :height (cdr face) :slant 'oblique))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (custom-theme-set-faces
   'user
   '(line-number ((t (:inherit fixed-pitch))))
   '(line-number-current-line ((t (:inherit fixed-pitch))))
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-checkbox ((t (:inherit 'fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-formula ((t (:inherit (fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "RoyalBlue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   )
  )

(defun my-org-mode-setup ()
  "Setup all org modes."
  ;; (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (setq-default org-M-RET-may-split-line nil
                org-cycle-include-plain-lists 'integrate
                org-enforce-todo-checkbox-dependencies t
                org-enforce-todo-dependencies t
                org-hide-emphasis-markers t
                org-return-follows-link t
                org-src-fontify-natively t
                org-src-preserve-indentation t
                org-startup-indented t
                org-catch-invisible-edits 'error
                org-link-frame-setup '((file . find-file)))

  ;; if variable-pitch-mode enabled just use my default font
  (set-face-attribute 'variable-pitch nil :family "Source Code Pro")

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("rs" . "src rust"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))

  (setq-default org-image-actual-width nil)

  (add-hook 'org-mode-hook 'org-appear-mode)
  (add-hook 'org-mode-hook 'flyspell-mode))

;; toggle visibilty of hidden elements
(use-package org-appear
  :ensure t
  :after org
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(use-package org
  :hook (org-mode . my-org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq-default org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; NOTE: in LISP (') a single quote treat as a "list" not a "function call".
  ;; scheduling an agenda on 'org-agenda'
  ;; Repeated Tasks - https://orgmode.org/manual/Repeated-tasks.html#Repeated-tasks
  (setq org-agenda-files
        '("~/.config/emacs/OrgFiles/Task.org"
          "~/.config/emacs/OrgFiles/Notes.org"
          "~/.config/emacs/OrgFiles/Habits.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq-default org-habit-graph-column 60)
  (setq-default org-refile-targets
                '(("Archive.org" :maxlevel . 1)
                  ("Task.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Custom TODO states and Agendas
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("batch" . ?b)
          ("idea" . ?i)
          ("note" . ?n)
          ("planning" . ?p)
          ("publish" . ?P)))

  ;;Agenda query documentation: https://orgmode.org/manual/Custom-Agenda-Views.html#Custom-Agenda-Views
  ;; Configure custom agenda views
  (setq-default org-agenda-custom-commands
                '(("d" "Dashboard"
                   ((agenda "" ((org-deadline-warning-days 7)))
                    (todo "NEXT"
                          ((org-agenda-overriding-header "Next Tasks")))
                    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

                  ("n" "Next Tasks"
                   ((todo "NEXT"
                          ((org-agenda-overriding-header "Next Tasks")))))

                  ;; '-' minus sign with tag name to remove 'email' in a list
                  ("W" "Work Tasks" tags-todo "+work-email")

                  ;; Low-effort next actions
                  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
                   ((org-agenda-overriding-header "Low Effort Tasks")
                    (org-agenda-max-todos 20)
                    (org-agenda-files org-agenda-files)))

                  ("w" "Workflow Status"
                   ((todo "WAIT"
                          ((org-agenda-overriding-header "Waiting on External")
                           (org-agenda-files org-agenda-files)))
                    (todo "REVIEW"
                          ((org-agenda-overriding-header "In Review")
                           (org-agenda-files org-agenda-files)))
                    (todo "PLAN"
                          ((org-agenda-overriding-header "In Planning")
                           (org-agenda-todo-list-sublevels nil)
                           (org-agenda-files org-agenda-files)))
                    (todo "BACKLOG"
                          ((org-agenda-overriding-header "Project Backlog")
                           (org-agenda-todo-list-sublevels nil)
                           (org-agenda-files org-agenda-files)))
                    (todo "READY"
                          ((org-agenda-overriding-header "Ready for Work")
                           (org-agenda-files org-agenda-files)))
                    (todo "ACTIVE"
                          ((org-agenda-overriding-header "Active Projects")
                           (org-agenda-files org-agenda-files)))
                    (todo "COMPLETED"
                          ((org-agenda-overriding-header "Completed Projects")
                           (org-agenda-files org-agenda-files)))
                    (todo "CANC"
                          ((org-agenda-overriding-header "Cancelled Projects")
                           (org-agenda-files org-agenda-files)))))))

  (setq-default org-capture-templates
                `(("t" "Tasks / Projects")
                  ("tt" "Task" entry (file+olp "~/.config/emacs/OrgFiles/Task.org" "Inbox")
                   "* TODO %?\n %U\n %a\n  %i" :empty-lines 1)
                  ;;("ts" "Clocked Entry Subtask" entry (clock)
                  ;;"* TODO %?\n %U\n %a\n %i" :empty-lines 1)

                  ("j" "Journal Entries")
                  ("jj" "Journal" entry
                   (file+olp+datetree "~/.config/emacs/OrgFiles/Journal.org")
                   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
                   ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
                   :clock-in :clock-resume
                   :empty-lines 1)
                  ("jm" "Meeting" entry
                   (file+olp+datetree "~/.config/emacs/OrgFiles/Journal.org")
                   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
                   :clock-in :clock-resume
                   :empty-lines 1)

                  ("w" "Workflows")
                  ("we" "Checking Email" entry (file+olp+datetree "~/.config/emacs/OrgFiles/Journal.org")
                   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

                  ("m" "Metrics Capture")
                  ("mw" "Weight" table-line (file+headline "~/.config/emacs/OrgFiles/Metrics.org" "Weight")
                   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  ;; Bindings straight to 'org-capture' - journal templates
  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (my-org-font-setup))

;; enable org-babel-do-load language for Rust
(use-package ob-rust
  :ensure t)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (js . t)
     (python . t)
     (rust . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))
;;; predefine Lsp
;; Rust mode
(use-package rust-mode
  :ensure t
  :hook ((rust-mode . flycheck-mode)
         (rust-mode . hs-minor-minor))
  :bind (:map rust-mode-map
              ("<f6>" . my/rust-format-buffer)
              ("C-c 6" . my/rust-format-buffer))
  :config
  ;;(require 'lsp-rustfmt)
  ;;https://gist.github.com/ntBre/b0622cc212314d4d2ea795d913980068
  (defun my/rust-format-buffer ()
    "Rust Format buffer"
    (interactive)
    (rust-format-buffer)
    (save-buffer)))
;;; Lsp
(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action)
              ;; ("C-c f" . flycheck-list-error)
              )

  :config
  ;;======================================
  ;; lsp config general begin
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-links nil
        lsp-keep-workspace-alive nil)
  ;; prevent perfomance to be slow
  (setq lsp-log-io nil)

  (cl-defmethod lsp-clients-extract-signature-on-hover
    (contents (_server-id (eql rust-analyzer)))
    "from https://github.com/emacs-lsp/lsp-mode/pull/1740 to extract signature in rust"
    (-let* (((&hash "value") contents)
            (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
            (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                           (-third-item groups)
                         (car groups)))
            (sig (--> sig_group
                      (--drop-while (s-equals? "```rust" it) it)
                      (--take-while (not (s-equals? "```" it)) it)
                      (--map (s-trim it) it)
                      (s-join " " it))))
      (lsp--render-element (concat "```rust\n" sig "\n```"))))

  (setq lsp-session-file "~/.config/emacs/etc/.lsp-session-v1")

  ;; lsp config general end
  ;;======================================

  ;; rust on lsp config
  (require 'lsp-rust)
  (setq lsp-rust-analyzer-completion-add-call-parenthesis nil
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-watch-command "clippy")
  )
;;; markdown setup
(dolist (hook '(markdown-mode))
  (add-hook hook (lambda () (flyspell-mode 1)))
  (setq markdown-hide-markup t))

;;; LaTeX setup
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  ( setq  org-latex-pdf-process
    '( "%latex -shell-escape -interaction nonstopmode -output-directory %o %f" )))
;;; TESTING CODE
;;=================================================
;; Elisp
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             ;; Use spaces, not tabs.
;;             (setq indent-tabs-mode nil)
;;             ;; Keep M-TAB for `completion-at-point'
;;             (define-key flyspell-mode-map "\M-\t" nil)
;;             ;; Pretty-print eval'd expressions.
;;             (define-key emacs-lisp-mode-map
;;               "\C-x\C-e" 'pp-eval-last-sexp)
;;             ;; Recompile if .elc exists.
;;             (add-hook (make-local-variable 'after-save-hook)
;;                       (lambda ()
;;                         (byte-force-recompile default-directory)))
;;             (define-key emacs-lisp-mode-map
;;               "\M-\r" 'reindent-then-newline-and-indent)))

;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; ;; Requires Ispell
;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

;; theme config
;;(load-theme 'tango-dark t)
;;=================================================

;;=================================================
;; Load custom theme
;; (add-to-list 'custom-theme-load-path "~/.config/emacs/theme/my-solarized.el")
;; (add-to-list 'load-path "~/.config/emacs/theme/my-solarized.el")
;; (defun my-load-theme (&optional frame)
;;   (with-selected-frame (or frame (selected-frame))
;;     (load-theme 'my-solarized t)))
;; (my-load-theme)
;; (add-hook 'after-make-frame-functions 'my-load-theme)

;; (add-to-list 'custom-theme-load-path "~/.config/emacs/theme")
;; (add-to-list 'load-path "~/.config/emacs/theme")
;; (defun my-load-theme (&optional frame)
;;   (with-selected-frame (or frame (selected-frame))
;;     (load-theme 'my-solarized t)))
;; (my-load-theme)
;; (add-hook 'after-make-frame-functions 'my-load-theme)
;;=================================================

;;; init.el ends here
