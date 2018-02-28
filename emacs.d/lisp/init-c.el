(use-package cc-mode
  :mode ("\\.c\\'" . cc-mode)
  :config
  (setq c-default-style "linux"
        c-basic-offset 4))


(provide 'init-c)
