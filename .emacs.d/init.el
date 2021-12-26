(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; disable visible scroolbal
(tool-bar-mode -1)      ; disable the toolbar
(tooltip-mode -1)       ; disable tooltips
(menu-bar-mode -1)      ; disabnle menu bar
(set-fringe-mode 10)    ; give some breathing

;; set up the visible bell
(setq visible-bell t)

;; launch emacs as full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; FONT CONFIGURATION ------------------------------------------------------------
;;(set-face-attribute 'default nil :font "Source Code Pro" :pixelsize=13)
(set-face-attribute 'default nil :font "Source Code Pro" :height 90)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 90)

;; Set the variables pitch face
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height 90 :weight 'regular)

;; set theme-colors
;;(load-theme 'tango-dark)
;;(load-theme 'wombat)


;;XXX TODO XXX: if the split buffer is '*Warning*', *Backtrace*' or
;;'*Messages', etc; just split the window below with 'height < 15'
;;(defun test-split ()
  ;;(interactive)
  ;;(split-window-below 45)
  ;;(save-selected-window
    ;;(select-window (next-window))
    ;;(beginning-of-buffer)
    ;;(re-search-forward "defun test-fun ")))

;; Make ESC quit prompts either to 'C-g'
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; PACKAGE MANAGER CONFIGURATION -------------------------------------------------
;; Initialize package sources
(require 'package)

;; enable line numbers as relative numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;;===============================================================
;; XXX RECOMMEND not using lambdas, instead create a function XXX
(defun do-not-display-line-number-mode-hook()
  "Hook to disable line-number-mode."
  (display-line-numbers-mode 0))

;; FIXME: 'add-hook' does not work!! to remove line-numbers
;;(add-hook 'org-mode-hook 'do-not-display-line-numbers-mode-hook)
;;(add-hook 'term-mode-hook 'do-not-display-line-numbers-mode-hook)
;;(add-hook 'eshel-mode-hook 'do-not-display-line-numbers-mode-hook)
;;(add-hook 'shell-mode-hook 'do-not-display-line-numbers-mode-hook)

;; OR
;;(dolist (mode '(org-mode-hook
;;		term-mode-hook
;;		shell-mode-hook
;;		eshell-mode-hook))
;;  (add-hook mode 'do-not-display-line-numbers-mode-hook))
;;===============================================================

;; Disable line numbers for some modes
;;org-mode-hook
(dolist (mode '(term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("Org" . "https://orgmode.org/elpa/")))
			;;("elpa" . "https://elpa.gnu.org/
			;;("melpa-stable" . "https://stable.melpa.org//pakages/")

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-packages on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; shows all used keybinding
;; to use this press: M-x 'clm/toggle-command-log-buffer'
(use-package command-log-mode)

;; enable global-command-log-mode
;; by hit: M-x global-command-log-mode

;; install 'diminish'
(use-package diminish
  :ensure t)

;; use Ivfy setup and Counsel for completion
(use-package ivy
  :diminish
  :demand
  :bind (;;("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Fri-Dec-17-2021
;; key binding in emacs
;; (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
;; (define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 12)))

(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 ("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-s" . counsel-grep-or-swiper)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))

  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; EXAMPLE:
;; "C-s" 'counsel-grep-or-swiper)
;;(general-define-key
;;"C-M-j" 'counsel-switch-buffer)

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Project")
    (setq projectile-project-search-path '("~/Project")))
  (setq proejctile-switch-project-action #'projecttile-dired))

;; most often 'counsel-projectile' package use for searching with 'rgrep'
;; with this 'counsel-projectile-rg' function
;; and you can bind it into another self buffer with combine key (C-c s r) + (C-c C-o)
;; and you would also possible to press key 'M-o' in every rgrep result
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; 'evil-magit' package was removed from MELPA
;; instead use 'evil-collection' that had installed already

;; Pull down all the information from git repository like 'issue',
;; 'pull-request', and respond them
(use-package forge)

;; 'efs': emacs_from_scratch
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
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
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾" )

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; NOTE: in LISP (') a single quote treat as a "list" not a "function call".
  ;; scheduling an agenda on 'org-agenda'
  ;; Repeated Tasks - https://orgmode.org/manual/Repeated-tasks.html#Repeated-tasks
  (setq org-agenda-files
	'("~/.emacs.d/OrgFiles/Task.org"
	  "~/.emacs.d/OrgFiles/Birthdays.org"
	  "~/.emacs.d/OrgFiles/Habits.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-refile-targets
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
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)))


  ;;Agenda query documentation: https://orgmode.org/manual/Custom-Agenda-Views.html#Custom-Agenda-Views
  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
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

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/.emacs.d/OrgFiles/Task.org" "Inbox")
	   "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	  ;;("ts" "Clocked Entry Subtask" entry (clock)
	   ;;"* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

	  ("j" "Journal Entries")
	  ("jj" "Journal" entry
	   (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
	   "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
	   ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
	   :clock-in :clock-resume
	   :empty-lines 1)
	  ("jm" "Meeting" entry
	   (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
	   "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
	   :clock-in :clock-resume
	   :empty-lines 1)

	  ("w" "Workflows")
	  ;;("we" "Checking Email" entry (file+olp+datetree ,(dw/get-todays-journal-file-name))
	  ("we" "Checking Email" entry (file+olp+datetree "~/.emacs.d/OrgFiles/Journal.org")
	   "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

	  ("m" "Metrics Capture")
	  ("mw" "Weight" table-line (file+headline "~/.emacs.d/OrgFiles/Metrics.org" "Weight")
	   "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))


  ;; Bindings straight to 'org-capture' - journal templates
  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (setq org-clock-sound "~/Downloads/ding.wav")

  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  ;;visual-fill-column-center-text t
  (setq visual-fill-column-width 120)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


;; XXX Added by SYSTEM XXX
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(visual-fill visual-fill-mode visual-fill-column visual-fill-column-mode org-bullets evil-surround forge evil-magit counsel-projectile which-key use-package rainbow-delimiters projectile ivy-rich hydra helpful general evil-collection doom-themes doom-modeline diminish-buffer diminish counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; XXX CHANGING TAB-FACES XXX
;; inherit the face of 'doom-modeline-panel' for better appearance
(set-face-attribute 'tab-bar-tab nil :inherit 'doom-modeline-panel :foreground nil :background nil)

;; only show the 'tab-bar' if there are 2 or more tabs
(setq tab-bar-show 1)
