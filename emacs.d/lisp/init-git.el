(require-package 'git-commit)
;; (require-package 'git-blame)
;; (require-package 'gitignore-mode)
;; (require-package 'gitconfig-mode)
;; (require-package 'git-messenger) ;; Though see also vc-annotate's "n" & "p" bindings
;; (require-package 'git-timemachine)



(when (maybe-require-package 'magit)
  (setq-default
   magit-process-popup-time 10
   magit-diff-refine-hunk t
   magit-completing-read-function 'magit-ido-completing-read)

  ;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)
  (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace))

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)

(require-package 'evil-magit)
(with-eval-after-load 'magit
  (require 'evil-magit))



(provide 'init-git)
