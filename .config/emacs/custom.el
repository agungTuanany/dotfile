(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(diff-hl org-bullets magit-delta eros org-appear forge all-the-icons flycheck rust-mode lsp-mode ob-rust which-key doom-modeline pdf-tools company-box consult orderless vertico marginalia company treemacs winum evil-nerd-commenter evil-surround evil-collection rainbow-delimiters doom-themes use-package evil magit))
 '(safe-local-variable-values '((eval local-set-key (kbd "C-c i") #'consult-outline))))
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "dim gray" :slant italic :weight semibold :height 0.85))))
 '(line-number ((t (:inherit fixed-pitch))))
 '(line-number-current-line ((t (:inherit default :foreground "#CFC0C5" :slant normal :weight bold))))
 '(marginalia-documentation ((t (:inherit nil :foreground "LemonChiffon4" :weight normal))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-checkbox ((t (:inherit 'fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-formula ((t (:inherit (fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(show-paren-match ((t (:underline t :weight ultra-bold))))
 '(whitespace-tab ((t (:foreground "#636363")))))
