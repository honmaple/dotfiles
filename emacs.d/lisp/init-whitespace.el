(use-package whitespace
  :defer t
  :diminish whitespace-mode "ⓦ"
  :init
  (progn
    (setq spacemacs-show-trailing-whitespace t))
  (defun maple/show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil
                        :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace 1))
  ;; (add-hook 'prog-mode-hook 'maple/show-trailing-whitespace)
  (defun maple/no-trailing-whitespace ()
    "Turn off display of trailing whitespace in this buffer."
    (setq show-trailing-whitespace nil))
  (dolist (hook '(prog-mode-hook
                  special-mode-hook
                  Info-mode-hook
                  eww-mode-hook
                  term-mode-hook
                  comint-mode-hook
                  compilation-mode-hook
                  twittering-mode-hook
                  minibuffer-setup-hook))
    (add-hook hook #'maple/no-trailing-whitespace))
  )

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t)
  :config
  (progn
    (global-set-key [remap just-one-space] 'cycle-spacing)
    ))

(provide 'init-whitespace)
