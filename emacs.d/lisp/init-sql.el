(use-package sql
  :defer t
  :init
  (progn
    (setq-default sql-input-ring-file-name (concat maple-cache-directory "sql_history")))
  :config
  (progn
    (add-hook 'sql-interactive-mode-hook
              (lambda () (toggle-truncate-lines t)))
    ;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
    (defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
      (unless (eq 'oracle sql-product)
        (sql-product-font-lock nil nil)))
    ;; (when (featurep 'evil) (evil-set-initial-state 'sql-interactive-mode 'insert))
    (add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)
    (add-hook 'sql-interactive-mode-hook 'maple/close-process)
    )
  :evil-emacs sql-interactive-mode)


(use-package sql-indent
  :ensure t
  :defer t)

(provide 'init-sql)
