(use-package lua-mode
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
  :init
  (progn
    (defun maple//go-set-tab-width ()
      "Set the tab width."
      (setq-local tab-width go-tab-width))
    (add-hook 'go-mode-hook 'maple//go-set-tab-width))
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)))


(use-package company-go
  :ensure t
  :defer t
  :config (maple/add-to-company-backend '(company-go) 'go-mode-hook))


(provide 'init-go)
