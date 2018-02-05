(use-package markdown-mode
  :ensure t
  :diminish orgtbl-mode
  :mode ("\\.m[k]d" . markdown-mode)
  :defer t
  :config
  (progn
    (require 'org-table)
    (defun cleanup-org-tables ()
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "-+-" nil t) (replace-match "-|-"))))
    (add-hook 'markdown-mode-hook 'orgtbl-mode)
    (add-hook 'markdown-mode-hook
              (lambda()
                (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))
    ))

;; (use-package markdown-preview-mode
;;   :load-path "site-lisp/markdown-preview/")

;; (use-package olivetti
;;   :defer t
;;   :init (add-hook 'org-mode-hook 'olivetti-mode))

(use-package markdown-toc
  :ensure t
  :defer t)

(provide 'init-text)
