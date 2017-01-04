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
    (defvar syntax-checking-use-original-bitmaps nil
      "If non-nil, use the original bitmaps from flycheck.")
    (when (and (fboundp 'define-fringe-bitmap)
               (not syntax-checking-use-original-bitmaps))
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (let ((bitmap (if syntax-checking-use-original-bitmaps
                      'flycheck-fringe-bitmap-double-arrow
                    'my-flycheck-fringe-indicator)))
      (flycheck-define-error-level 'error
        :severity 2
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-error)
      (flycheck-define-error-level 'warning
        :severity 1
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-warning)
      (flycheck-define-error-level 'info
        :severity 0
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-info))
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
      )
    (push '("^\\*Flycheck.+\\*$"
            :regexp t
            :dedicated t
            :position bottom
            :stick t
            :noselect t)
          popwin:special-display-config)
    ))

(defun maple/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
    If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))
(provide 'init-flycheck)
