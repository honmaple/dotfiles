(use-package go-mode
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
  :evil-bind
  (normal go-mode-map
          [f6] 'maple/gofmt
          "gd" 'godef-jump))

(use-package company-go
  :init (maple/company-backend 'go-mode-hook 'company-go)
  :config
  (setq company-go-gocode-command "/home/jianglin/go/bin/gocode"))

(provide 'init-go)
