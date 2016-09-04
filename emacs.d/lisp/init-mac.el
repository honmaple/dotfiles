(require-package 'exec-path-from-shell)


(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

(provide 'init-mac)
