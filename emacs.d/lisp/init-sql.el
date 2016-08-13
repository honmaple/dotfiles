(require-package 'sql-indent)

(use-package sql
  :defer t
  :init
  (progn
    (setq-default sql-input-ring-file-name
                  (expand-file-name ".sqli_history" user-emacs-directory)))
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
    (add-hook 'sql-interactive-mode-hook 'sanityinc/never-indent)
    (add-hook 'sql-interactive-mode-hook
              (lambda () (toggle-truncate-lines t)))
    (after-load 'page-break-lines
      (push 'sql-mode page-break-lines-modes))

    ;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
    (defun sanityinc/font-lock-everything-in-sql-interactive-mode ()
      (unless (eq 'oracle sql-product)
        (sql-product-font-lock nil nil)))
    (add-hook 'sql-interactive-mode-hook 'sanityinc/font-lock-everything-in-sql-interactive-mode)
    ))

(use-package sql-indent
  :defer t)




(provide 'init-sql)
