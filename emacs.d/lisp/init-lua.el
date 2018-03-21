(use-package lua-mode
  :diminish lua-mode
  :config
  (setq lua-indent-level 4
        lua-indent-string-contents t))

(use-package company-lua
  :init (maple/company-backend 'lua-mode-hook 'company-lua))

(provide 'init-lua)
;;; init-lua.el ends here
