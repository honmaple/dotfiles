(use-package sh-script
  :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . sh-mode))
    (add-to-list 'auto-mode-alist '("\\.bash_history\\'" . sh-mode))
    (add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
    (add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
    (add-to-list 'auto-mode-alist '("\\.bashrc.local\\'" . sh-mode))
    (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
    (add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
    ))

;; (require-package 'shell-pop)

;; (use-package shell-pop
;;   :defer t
;;   :init
;;   (progn
;;     (setq shell-pop-term-shell "/bin/bash"
;;           shell-pop-window-size 30
;;           shell-pop-window-position "bottom")
;;     ))


(provide 'init-shell)
