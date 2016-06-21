(require-package 'cc-mode)

(use-package cc-mode
  :defer t
  ;; :mode ("\\.c\\'" . cc-mode)
  :config
  (progn
    (setq c-default-style "linux")
    ;; (add-to-list 'auto-mode-alist `("\\.h$" . ,c-c++-default-mode-for-headers))
    ))
;; (with-eval-after-load 'c-mode
;;   (add-to-list 'auto-mode-alist `("\\.h$" . ,c-c++-default-mode-for-headers))
;;   (require 'compile)
;;   (c-toggle-auto-newline 1)

(provide 'init-c)
