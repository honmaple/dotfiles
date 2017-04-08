(use-package sh-script
  :defer t
  :mode
  (("\\.bash_profile\\'" . sh-mode)
   ("\\.bash_history\\'" . sh-mode)
   ("\\.sh\\'" . sh-mode)
   ("\\.bash\\'" . sh-mode)
   ("\\.bashrc.local\\'" . sh-mode)
   ))

(use-package multi-term
  :ensure t
  :defer t
  :config
  (progn
    (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
    ))

(use-package shell-pop
  :ensure t
  :defer t
  :init
  (progn
    (defvar shell-default-shell (if (eq window-system 'w32)
                                    'eshell
                                  'ansi-term)
      "Default shell to use in Spacemacs. Possible values are `eshell', `shell',
    `term' and `ansi-term'.")
    (setq comint-prompt-read-only t)
    (setq shell-pop-window-position "bottom"
          shell-pop-window-size 30
          shell-pop-term-shell "/bin/bash"
          shell-pop-full-span t)
    (add-hook 'term-mode-hook 'maple/close-process)
    (add-hook 'term-mode-hook (lambda () (nlinum-mode -1)))
    (defmacro make-shell-pop-command (func &optional shell)
      "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
      (let* ((name (symbol-name func)))
        `(defun ,(intern (concat "maple/shell-pop-" name)) (index)
           ,(format (concat "Toggle a popup window with `%S'.\n"
                            "Multiple shells can be opened with a numerical prefix "
                            "argument. Using the universal prefix argument will "
                            "open the shell in the current buffer instead of a "
                            "popup buffer.") func)
           (interactive "P")
           (require 'shell-pop)
           (if (equal '(4) index)
               ;; no popup
               (,func ,shell)
             (shell-pop--set-shell-type
              'shell-pop-shell-type
              (backquote (,name
                          ,(concat "*" name "*")
                          (lambda nil (,func ,shell)))))
             (shell-pop index)))))
    (make-shell-pop-command eshell)
    (make-shell-pop-command term shell-pop-term-shell)
    (make-shell-pop-command multiterm)
    (make-shell-pop-command ansi-term shell-pop-term-shell)
    (defun maple/default-pop-shell ()
      "Open the default shell in a popup."
      (interactive)
      (let ((shell (if (eq 'multi-term shell-default-shell)
                       'multiterm
                     shell-default-shell)))
        (call-interactively (intern (format "maple/shell-pop-%S" shell)))))
    (defun term-send-tab ()
      "Send tab in term mode."
      (interactive)
      (term-send-raw-string "\t"))
    (evil-define-key 'normal term-raw-map "p" 'term-paste)
    (evil-define-key 'insert term-raw-map
      (kbd "C-c C-d") 'term-send-eof
      (kbd "C-c C-z") 'term-stop-subjob
      (kbd "<tab>") 'term-send-tab
      (kbd "C-k") 'term-send-up
      (kbd "C-j") 'term-send-down)
    ))

(use-package xterm-color
  :ensure t
  :defer t
  :config
  (progn
    ;; Comint and Shell
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))))


(provide 'init-shell)
