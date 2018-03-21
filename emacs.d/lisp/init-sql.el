(use-package sql
  :ensure nil
  :config
  (setq sql-input-ring-file-name (concat maple-cache-directory "sql.history"))
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (database :default "postgres")
          (server :default "localhost")
          (port :default 5432)))
  (add-hook 'sql-interactive-mode-hook 'maple/truncate-lines)
  (add-hook 'sql-interactive-mode-hook 'maple/close-process)
  :evil-state (sql-interactive-mode . insert))

(use-package sql-indent)

(provide 'init-sql)
