(require-package 'evil)
(evil-mode 1)
; (define-key evil-insert-state-map (kbd "jj") 'evil-normal-state)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "C-h") 'evil-window-left)
(define-key evil-insert-state-map (kbd "C-h") (kbd "<left>"))
(define-key evil-insert-state-map (kbd "C-l") (kbd "<right>"))
(define-key evil-normal-state-map (kbd "H") (kbd "^"))
(define-key evil-normal-state-map (kbd "L") (kbd "$"))
(define-key evil-visual-state-map (kbd "H") (kbd "^"))
(define-key evil-visual-state-map (kbd "L") (kbd "$"))
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
; (require-package 'ranger)
; (setq ranger-excluded-extensions '("mkv" "iso" "mp4" "pyc"))

(require-package 'evil-surround)
(global-evil-surround-mode 1)

(require-package 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "cc" 'comment-or-uncomment-region
  "'"  'shell-pop
  "ff" 'counsel-find-file
  "fr" 'ivy-recentf
  "fw" 'counsel-ag
  "u" 'undo-tree-visualize
  )

(require-package 'evil-matchit)
(global-evil-matchit-mode 1)

(require-package 'evil-escape)
(require 'evil-escape)
(hl-line-mode 1)  ;; !!:must set the hl-line-mode or it may not work
(setq-default evil-escape-key-sequence "jj")
(setq-default evil-escape-delay 0.2)
(setq evil-escape-excluded-major-modes '(dired-mode neotree-mode evil-visual-state))
(evil-escape-mode 1)

(require-package 'neotree)
(global-set-key [f2] 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(provide 'init-evil)
