(use-package markdown-mode
  :config
  (progn
    (use-package org-table
      :ensure nil
      :diminish orgtbl-mode
      :hook (markdown-mode . orgtbl-mode))

    (defun cleanup-org-tables ()
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "-+-" nil t) (replace-match "-|-"))))
    (add-hook 'markdown-mode-hook
              (lambda()
                (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)))
    )
  :bind
  (:map markdown-mode-map
        ([f5] . markdown-toggle-markup-hiding)))

;; (use-package markdown-preview-mode
;;   :after (markdown-mode)
;;   :demand t
;;   :load-path "site-lisp/markdown-preview")

;; (use-package olivetti
;;   :defer t
;;   :init (add-hook 'org-mode-hook 'olivetti-mode))

(use-package markdown-toc)

(use-package yaml-mode)

(provide 'init-text)
