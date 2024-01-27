;;; tuanany-evil.el.-- File _*_ lexical-binding: t _*_
;; Copyright (C) 2021-2023 agung Tuanany 
;; Author: Agung Tuanany <agung.tuananydotgmail.com>
;; URL:
;; Package-Requires:

;; This file is nost part of GNU emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 or the License, or any later version.

;; This program is distributed in the hope it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNES FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have recieved a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:
(use-package evil
  :ensure t
  :init
  ;; going for emacs state whenever it makes sense
  ;; respect and use emacs motion state (emacs movement)
  (setq evil-default-state 'emacs
	evil-emacs-state-modes nil
	evil-insert-state-modes nil
	evil-motion-state-modes nil
	evil-normal-state-modes '(text-mode prog-mode fundamental-mode
					    css-mode conf-mode TeX-mode
					    diff-mode))
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (add-hook 'view-mode-hook 'evil-emacs-state)

  ;; base behavior adjustment
  (setq evil-cross-lines t
	evil-ex-substitute-global t   ;regex use 'g' at all matched
	evil-kill-on-visual-paste nil
	evil-move-beyond-eol t
	;;evil-default-cursor 'hbar
	evil-move-cursor-back nil
	evil-symbol-word-search t
	evil-undo-system 'undo-tree
	evil-want-C-i-jump t
	evil-want-C-u-scroll t
	evil-want-C-h-delete t
	evil-want-fine-undo t
	evil-want-integration t
	evil-want-keybinding nil
	)

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
  ;; `keymap-set' Emacs-29 features
  (keymap-set evil-insert-state-map "C-g" 'evil-normal-state)
  (keymap-set evil-normal-state-map "v" 'evil-visual-block)
  (keymap-set evil-normal-state-map "C-v" 'evil-visual-char)
  (keymap-set evil-visual-state-map "v" 'evil-visual-block)
  (keymap-set evil-visual-state-map "C-v" 'evil-visual-char)

  ;; Use visual line motion even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;;(evil-set-initial-state 'message-buffer-mode 'normal)
  ;;(evil-set-initial-state 'dashboard-mode 'normal)

  ;; Evil Leader Keys
  (evil-set-leader nil (kbd "C-,"))
  ;;(evil-set-leader 'normal "," t)
  (evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>lp") 'list-packages)
  (evil-define-key 'normal 'global (kbd "<leader>ev") 'eval-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>ev") 'list-ibuffer)
  (evil-define-key 'insert 'global (kbd "TAB") 'tab-to-tab-stop)
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
