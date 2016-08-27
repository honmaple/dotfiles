(require-package 'sql-indent)

(use-package sql
  :defer t
  :init
  (progn
    (setq-default sql-input-ring-file-name
                  (expand-file-name ".sqli_history" user-emacs-directory))
    )
  :config
  (progn
    (defun sanityinc/pop-to-sqli-buffer ()
      "Switch to the corresponding sqli buffer."
      (interactive)
      (if sql-buffer
          (progn
            (pop-to-buffer sql-buffer)
            (goto-char (point-max)))
        (sql-set-sqli-buffer)
        (when sql-buffer
          (sanityinc/pop-to-sqli-buffer))))

    (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)
    ;; (add-hook 'sql-interactive-mode-hook 'sanityinc/never-indent)
    (add-hook 'sql-interactive-mode-hook
              (lambda () (toggle-truncate-lines t)))
    (after-load 'page-break-lines
      (push 'sql-mode page-break-lines-modes))

    ;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
    (defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
      (unless (eq 'oracle sql-product)
        (sql-product-font-lock nil nil)))
    (add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)


    (defun sql-stop (process event)
      "Called when the SQL process is stopped.

    Writes the input history to a history file using
    `comint-write-input-ring' and inserts a short message in the SQL buffer.

    This function is a sentinel watching the SQL interpreter process.
    Sentinels will always get the two parameters PROCESS and EVENT."
      (comint-write-input-ring)
      (if (and (eq (current-buffer) sql-buffer)
               (not buffer-read-only))
          (insert (format "\nProcess %s %s\n" process event))
        (message "Process %s %s" process event))
      (kill-this-buffer)(delete-window))
    ;; (set-process-sentinel (get-buffer-process (current-buffer)) 'maple/sql-stop)
    ;; (set-process-sentinel (get-buffer-process (current-buffer)) 'sql-stop)
    ))


(use-package sql-indent
  :defer t)




(provide 'init-sql)
