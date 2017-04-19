;;; init-git.el --- git stuff

;;; Commentary:
;;


;;; Code:
(require-package 'magit)
(require-package 'evil-magit)
(require-package 'git-commit)
(require-package 'git-timemachine)

(after-load 'magit
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
    (add-hook 'magit-popup-mode-hook 'maple/no-trailing-whitespace)
    (fullframe magit-status magit-mode-quit-window))
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)
   :map magit-mode-map
   ("C-M-<up>" . magit-section-up)
   ("<tab>" . magit-section-cycle)
   ("C-<tab>" . magit-section-toggle)))

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

(use-package git-gutter-fringe
  :ensure t
  :commands git-gutter-mode
  :diminish git-gutter-mode
  :init
  (progn
    (with-eval-after-load 'git-gutter
      (require 'git-gutter-fringe))
    (setq git-gutter-fr:side 'right-fringe)
    (add-hook 'after-init-hook #'global-git-gutter-mode)
    ;; (global-git-gutter-mode t)
    )
  :config
  (progn
    ;; custom graphics that works nice with half-width fringes
    (fringe-helper-define 'git-gutter-fr:added nil
      "..X...."
      "..X...."
      "XXXXX.."
      "..X...."
      "..X...."
      )
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "......."
      "......."
      "XXXXX.."
      "......."
      "......."
      )
    (fringe-helper-define 'git-gutter-fr:modified nil
      "..X...."
      ".XXX..."
      "XX.XX.."
      ".XXX..."
      "..X...."
      )))

(provide 'init-git)
