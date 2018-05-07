(use-package sh-script
  :ensure nil
  :mode
  (("\\.bash_profile\\'" . sh-mode)
   ("\\.bash_history\\'" . sh-mode)
   ("\\.sh\\'" . sh-mode)
   ("\\.bash\\'" . sh-mode)
   ("\\.bashrc.local\\'" . sh-mode)
   ))

(use-package multi-term
  :config
  (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab)))

(use-package shell-pop
  :init
  (setq shell-pop-window-position "bottom"
        shell-pop-window-size 30
        shell-pop-term-shell "/bin/bash"
        shell-pop-full-span t
        shell-pop-shell-type (if maple-system-is-windows
                                 '("eshell" "*eshell*" (lambda () (eshell)))
                               '("ansi-term" "*ansi-term*"
                                 (lambda () (ansi-term shell-pop-term-shell)))))
  (add-hook 'term-mode-hook 'maple/close-process)
  (add-hook 'term-mode-hook (lambda () (nlinum-mode -1)))
  (defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))
  :evil-bind
  ((normal term-raw-map
           "p" 'term-paste)
   (insert term-raw-map
           (kbd "C-c C-d") 'term-send-eof
           (kbd "C-c C-z") 'term-stop-subjob
           (kbd "<tab>") 'term-send-tab
           (kbd "C-k") 'term-send-up
           (kbd "C-j") 'term-send-down)))

(use-package xterm-color
  :config
  ;; Comint and Shell
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))


(provide 'init-shell)
