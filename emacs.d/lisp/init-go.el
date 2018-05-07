;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint

(use-package go-mode
  :config
  (setq gofmt-show-errors nil)

  (use-package golint)
  (use-package go-eldoc
    :hook (go-mode . go-eldoc-setup))
  :evil-bind
  (normal go-mode-map
          [f6] 'gofmt
          "gd" 'godef-jump))

(use-package company-go
  :init (maple/company-backend 'go-mode-hook 'company-go))

(provide 'init-go)
