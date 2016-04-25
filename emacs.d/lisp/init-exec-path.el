(require-package 'exec-path-from-shell)

(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(defconst maple-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "maple storage area for persistent files")
(provide 'init-exec-path)
