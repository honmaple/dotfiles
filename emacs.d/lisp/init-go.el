(use-package lua-mode
  :ensure t
  :defer t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :init
  (progn
    (setq lua-indent-level 4
          lua-indent-string-contents t)))

(use-package company-lua
  :ensure t
  :defer t
  :config (maple/add-to-company-backend '(company-lua) 'lua-mode-hook))

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
    (defun maple/gofmt()
      (interactive)
      (gofmt)
      (untabify (point-min) (point-max)))
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    )
  :evil-bind
  (normal go-mode-map
          [f6] 'maple/gofmt))


(use-package company-go
  :ensure t
  :defer t
  :config (maple/add-to-company-backend '(company-go) 'go-mode-hook))


(provide 'init-go)
