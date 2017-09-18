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
  :mode-setq
  (go-mode
   tab-width 4
   indent-tabs-mode nil)
  :config
  (progn
    (setq gofmt-show-errors nil)
    (add-hook 'before-save-hook 'gofmt-before-save)
    ))


(use-package company-go
  :ensure t
  :defer t
  :config (maple/add-to-company-backend '(company-go) 'go-mode-hook))


(provide 'init-go)
