(use-package markdown-mode
  :config
  (use-package org-table
    :ensure nil
    :diminish orgtbl-mode
    :hook (markdown-mode . orgtbl-mode))

  (defun cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))
  (maple/add-hook 'markdown-mode-hook
    (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local))
  :bind
  (:map markdown-mode-map
        ([f5] . markdown-toggle-markup-hiding)))

(use-package markdown-preview-mode
  :after (markdown-mode)
  :commands (markdown-preview-mode)
  :load-path "site-lisp/markdown-preview")

;; (use-package olivetti
;;   :defer t
;;   :hook (org-mode . olivetti-mode))

(use-package markdown-toc)
(use-package yaml-mode)
(use-package vimrc-mode)
(use-package json-mode)

(provide 'init-text)
