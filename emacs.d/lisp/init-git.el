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
(require-package 'git-timemachine)

(with-eval-after-load 'magit
  (require 'evil-magit))

(use-package magit
  :defer t
  :config
  (progn
    (setq magit-completing-read-function 'magit-builtin-completing-read
          magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
    (setq-default
     magit-process-popup-time 10
     magit-diff-refine-hunk t)
    (add-hook 'magit-popup-mode-hook 'maple/no-trailing-whitespace))
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

(use-package git-timemachine
  :defer t
  :config
  (progn
    (eval-after-load 'git-timemachine
      '(progn
         (global-evil-mc-mode -1)
         (evil-make-overriding-map git-timemachine-mode-map 'normal)
         ;; force update evil keymaps after git-timemachine-mode loaded
         (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))
    ))

(provide 'init-git)
