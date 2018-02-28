;;; init-git.el --- git stuff

;;; Commentary:
;;


;;; Code:
(use-package evil-magit
  :demand t
  :after magit)

(use-package magit
  :config
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-process-popup-time 10
        magit-diff-refine-hunk t)
  (after-load 'fullframe
    (fullframe magit-status magit-mode-quit-window))
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch-popup)
   :map magit-mode-map
   ("C-M-<up>" . magit-section-up)
   ("<tab>" . magit-section-cycle)
   ("C-<tab>" . magit-section-toggle)))

(use-package git-commit
  :hook (git-commit-mode . goto-address-mode))

(use-package git-timemachine
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :hook (after-init . global-git-gutter-mode)
  :init
  (with-eval-after-load 'git-gutter
    (require 'git-gutter-fringe))
  (setq git-gutter-fr:side 'right-fringe)
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
