;;; Colourise CSS colour literals
;; (when (maybe-require-package 'rainbow-mode)
;;   (dolist (hook '(css-mode-hook html-mode-hook web-mode-hook sass-mode-hook))
;;     (add-hook hook 'rainbow-mode)))


;;; SASS and SCSS
(require-package 'css-mode)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'less-css-mode)

(use-package css-mode
  :defer t
  :init
  (progn
    (set (make-local-variable 'company-backends) '(company-css))
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
