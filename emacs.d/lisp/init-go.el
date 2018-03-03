(use-package company-lua)

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :diminish lua-mode
  :interpreter ("lua" . lua-mode)
  :init
  (setq lua-indent-level 4
        lua-indent-string-contents t)
  (maple/company-backend 'lua-mode-hook 'company-lua)
  )


(use-package company-go
  :config
  (setq company-go-gocode-command "/home/jianglin/go/bin/gocode"))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :setq
  (:mode go-mode
         tab-width 4
         indent-tabs-mode t)
  :config
  (setq gofmt-show-errors nil
        godef-command "/home/jianglin/go/bin/godef")

  (defun maple/gofmt()
    (interactive)
    (save-excursion
      ;; (untabify (point-min) (point-max))
      (gofmt)))
  (maple/company-backend 'go-mode-hook 'company-go)
  :evil-bind
  (normal go-mode-map
          [f6] 'maple/gofmt
          "gd" 'godef-jump))


(provide 'init-go)
