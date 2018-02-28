(use-package company-lua)

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :diminish lua-mode
  :interpreter ("lua" . lua-mode)
  :init
  (setq lua-indent-level 4
        lua-indent-string-contents t)
  (maple/add-to-company-backend '(company-lua) 'lua-mode-hook))


(use-package company-go
  :config
  (setq company-go-gocode-command "/home/jianglin/go/bin/gocode"))

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :mode-setq
  (go-mode
   tab-width 4
   indent-tabs-mode t)
  :config
  (setq gofmt-show-errors nil
        godef-command "/home/jianglin/go/bin/godef")

  (defun maple/gofmt()
    (interactive)
    (let ((p (point)))
      (gofmt)
      ;; (untabify (point-min) (point-max))
      (goto-char p)))
  (maple/add-to-company-backend '(company-go) 'go-mode-hook)
  :evil-bind
  (normal go-mode-map
          [f6] 'maple/gofmt
          "gd" 'godef-jump))


(provide 'init-go)
