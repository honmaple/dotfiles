;; 启动显示信息
(setq-default initial-scratch-message
              (if (executable-find "fortune")
                  (format
                   ";; %s\n\n"
                   (replace-regexp-in-string
                    "\n" "\n;; " ; comment each line
                    (replace-regexp-in-string
                     "\n$" ""    ; remove trailing linebreak
                     (shell-command-to-string "fortune"))))
                (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

(provide 'init-lisp)
