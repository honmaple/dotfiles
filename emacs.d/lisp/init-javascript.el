(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'coffee-mode)
(maybe-require-package 'company-tern)
(require-package 'rainbow-delimiters)


(use-package js2-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
  :config
  (progn
    (setq-default js2-basic-offset 2
                  js2-bounce-indent-p nil)
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    (setq-default js-indent-level 2)
    ))

;; Javascript nests {} and () a lot, so I find this helpful

(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
      (add-hook hook 'rainbow-delimiters-mode))
    ))

(use-package json-mode
  :defer t)

;;; Coffeescript
(use-package coffee-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode))
  :config
  (progn
    (setq coffee-js-mode preferred-javascript-mode
          coffee-tab-width preferred-javascript-indent-level)
    ))

(use-package company-tern
  :defer t
  :config
  (progn
    (after-load 'company
      (dolist (hook '(js-mode-hook
                      js2-mode-hook
                      js3-mode-hook
                      inferior-js-mode-hook
                      ))
        (add-hook hook
                  (lambda ()
                    (tern-mode t)
                    (make-local-variable 'company-backends)
                    (setq company-backends (copy-tree company-backends))
                    (setf (car company-backends)
                          (append '(company-tern) (car company-backends)))
                    )))
      )))

(provide 'init-javascript)
