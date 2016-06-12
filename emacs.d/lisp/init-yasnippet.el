(require-package 'yasnippet)

(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode)
  :diminish yas-minor-mode "ⓨ"
  :init
  (progn
    ;; (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode))
  :config
  (progn
    (defvar yas-global-mode nil)
    (setq yas-triggers-in-field t
          yas-wrap-around-region t
          helm-yas-display-key-on-candidate t)
    (setq yas-prompt-functions '(yas-completing-prompt))
    (setq yas-minor-mode-map (make-sparse-keymap))
    (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
    (setq maple/yasnippets (expand-file-name "~/.emacs.d/yasnippets"))
    (if (and  (file-exists-p maple/yasnippets) (not (member maple/yasnippets yas-snippet-dirs)))
        (add-to-list 'yas-snippet-dirs maple/yasnippets))
    ))
(provide 'init-yasnippet)
