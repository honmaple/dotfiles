(use-package cc-mode
  :ensure t
  :defer t
  ;; :mode ("\\.c\\'" . cc-mode)
  :config
  (progn
    (setq c-default-style "linux")
    (setq c-basic-offset 4)
    ;; (add-to-list 'auto-mode-alist `("\\.h$" . ,c-c++-default-mode-for-headers))
    ))
;; (after-load 'c-mode
;;   (add-to-list 'auto-mode-alist `("\\.h$" . ,c-c++-default-mode-for-headers))
;;   (require 'compile)
;;   (c-toggle-auto-newline 1)

(provide 'init-c)
