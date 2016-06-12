(require-package 'cc-mode)
(setq c-default-style "linux")
(with-eval-after-load 'c-mode
  (add-to-list 'auto-mode-alist `("\\.h$" . ,c-c++-default-mode-for-headers))
  (require 'compile)
  (c-toggle-auto-newline 1)
  )

(provide 'init-c)
