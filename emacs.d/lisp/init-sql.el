(use-package sql
  :defer t
  :init
  (setq sql-input-ring-file-name (concat maple-cache-directory "sql_history"))
  :config
  (add-hook 'sql-interactive-mode-hook
            (lambda () (toggle-truncate-lines t)))
  (add-hook 'sql-interactive-mode-hook 'maple/close-process)
  :evil-state (sql-interactive-mode . insert)
  :bind (:map sql-interactive-mode-map
              ("<up>" . comint-previous-input)
              ("<down>" . comint-next-input)
              ))


(use-package sql-indent
  :ensure t
  :defer t)

(provide 'init-sql)
