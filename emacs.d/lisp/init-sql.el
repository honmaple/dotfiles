(use-package sql
  :ensure nil
  :init
  (setq sql-input-ring-file-name (concat maple-cache-directory "sql_history"))
  :config
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
