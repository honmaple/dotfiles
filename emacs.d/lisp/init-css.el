;;; SASS and SCSS
(require-package 'css-mode)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'less-css-mode)

(use-package css-mode
  :defer t
  :config
  (progn
    (maple/add-to-company-backend '(company-css) 'css-mode-hook)
    (put 'css-indent-offset 'safe-local-variable #'integerp)
    ))

(use-package sass-mode
  :defer t
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :defer t
  :config (setq-default scss-compile-at-save nil)
  :mode ("\\.scss\\'" . scss-mode))


(use-package less-css-mode
  :defer t
  :mode ("\\.less\\'" . less-css-mode))

(provide 'init-css)
