(require-package 'ox-rst)
(require-package 'markdown-mode)

(use-package ox-rst
  :after org)

(use-package markdown-mode
  :defer t
  :config
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

(provide 'init-text)
