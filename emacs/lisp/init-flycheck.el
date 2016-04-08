(require-package 'flycheck-pos-tip) ;;显示tooltip
(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)

  ; (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)


  (defun maple/toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.
    If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (-if-let (window (flycheck-get-error-list-window))
             (quit-window nil window)
             (flycheck-list-errors)))

    (evil-leader/set-key
      "el" 'maple/toggle-flycheck-error-list
      "ec" 'flycheck-clear
      )

    (evil-define-key 'normal flycheck-error-list-mode-map
                     "q" 'quit-window
                     "j" #'flycheck-error-list-next-error
                     "k" #'flycheck-error-list-previous-error
                     "K" #'evil-previous-line
                     "J" #'evil-next-line
                     (kbd "RET") #'flycheck-error-list-goto-error)
    (diminish 'flycheck-mode "ⓢ"))

(with-eval-after-load 'flycheck
                      (flycheck-pos-tip-mode))

(provide 'init-flycheck)
