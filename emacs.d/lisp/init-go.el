(use-package lua-mode
  :ensure t
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :init
  (progn
    (setq lua-indent-level 2
          lua-indent-string-contents t)))

(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :init
  (progn
    (add-hook 'go-mode-hook
              (lambda ()
                (setq tab-width 4))
              ))
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)))


(use-package company-go
  :ensure t
  :defer t
  :config (maple/add-to-company-backend '(company-go) 'go-mode-hook))


(provide 'init-go)
