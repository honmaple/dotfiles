;; (require-package 'git-commit)
;; (require-package 'git-blame)
;; (require-package 'gitignore-mode)
;; (require-package 'gitconfig-mode)
;; (require-package 'git-messenger) ;; Though see also vc-annotate's "n" & "p" bindings
;; (require-package 'git-timemachine)
(require-package 'magit)
(require-package 'evil-magit)
(require-package 'fullframe)
(require-package 'git-commit)

(with-eval-after-load 'magit
  (require 'evil-magit))

(use-package magit
  :defer t
  :config
  (progn
    (setq-default
     magit-process-popup-time 10
     magit-diff-refine-hunk t
     magit-completing-read-function 'magit-ido-completing-read)
    (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace))
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)
   :map magit-mode-map
   ("C-M-<up>" . magit-section-up)
   ("<tab>" . magit-section-cycle)
   ("C-<tab>" . magit-section-toggle)))

(use-package fullframe
  :config
  (progn
    (after-load 'magit
      (fullframe magit-status magit-mode-quit-window)
      )))

(use-package git-commit
  :defer t
  :config
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

;; (use-package evil-magit
;;   :defer t)

(provide 'init-git)
