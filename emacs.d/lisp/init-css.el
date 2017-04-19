(use-package css-mode
  :ensure t
  :defer t
  :config
  (progn
    (maple/add-to-company-backend '(company-css) 'css-mode-hook)
    (put 'css-indent-offset 'safe-local-variable #'integerp)
    ))

(use-package sass-mode
  :ensure t
  :defer t
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :ensure t
  :defer t
  :config (setq-default scss-compile-at-save nil)
  :mode ("\\.scss\\'" . scss-mode))


(use-package less-css-mode
  :ensure t
  :defer t
  :mode ("\\.less\\'" . less-css-mode))

(provide 'init-css)
