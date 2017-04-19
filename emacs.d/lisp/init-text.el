(use-package ox-rst
  :ensure t
  :mode ("\\.org" . org-mode)
  :defer t)

(use-package markdown-mode
  :ensure t
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
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)
    ))

;; (use-package markdown-preview-mode
;;   :load-path "site-lisp/markdown-preview/")

(use-package markdown-toc
  :ensure t
  :defer t)

(provide 'init-text)
