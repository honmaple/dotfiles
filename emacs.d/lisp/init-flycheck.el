(require-package 'flycheck)
(require-package 'flycheck-pos-tip)  ;; 显示tooltip

(use-package flycheck
  :defer t
  :diminish flycheck-mode "ⓢ"
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
          flycheck-idle-change-delay 0.8)
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
    ))

(use-package flycheck-pos-tip
  :defer t
  :init
  (progn
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode)
      (evil-define-key 'normal flycheck-error-list-mode-map
        "q" 'quit-window
        "j" #'flycheck-error-list-next-error
        "k" #'flycheck-error-list-previous-error
        "K" #'evil-previous-line
        "J" #'evil-next-line
        (kbd "RET") #'flycheck-error-list-goto-error)
      )))

(defun maple/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
    If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))
(provide 'init-flycheck)
