;; Fill column indicator
;; (require-package 'fill-column-indicator)
;; (use-package fill-column-indicator
;;   :config
;;   (progn
;;     (setq fci-rule-column 80)
;;     (defun maple/prog-mode-fci-settings ()
;;       (turn-on-fci-mode)
;;       (when show-trailing-whitespace
;;         (set (make-local-variable 'whitespace-style) '(face trailing))
;;         (whitespace-mode 1)))
;;     ;; See:https://github.com/alpaker/Fill-Column-Indicator/issues/46
;;     (add-hook 'after-change-major-mode-hook
;;               (lambda () (if (string= major-mode "web-mode")
;;                         (turn-off-fci-mode) (turn-on-fci-mode))))
;;     (add-hook 'prog-mode-hook 'maple/prog-mode-fci-settings))
;;   )
(provide 'init-fci)
