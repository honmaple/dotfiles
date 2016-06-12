(require-package 'quickrun)
(use-package quickrun
             :defer t
             :config (evil-set-initial-state 'quickrun/mode 'emacs)
             )
;; (require 'quickrun)
;; (evil-set-initial-state 'quickrun/mode 'emacs)
;; (add-hook 'quickrun-mode-hook
;;           (lambda ()
;;             (evil-mode -1)
;;             ))
;;(with-eval-after-load 'quickrun
;; (push '("*quickrun*") popwin:special-display-config)
;;)
(provide 'init-quickrun)
