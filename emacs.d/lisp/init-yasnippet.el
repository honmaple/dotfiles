(require-package 'yasnippet)
(require-package 'dropdown-list)
(require 'yasnippet)


;; (yas-global-mode 1)

(yas-reload-all)

(eval-after-load 'yasnippet
  '(progn
     ;; We don't want undefined variable errors
     (require 'dropdown-list)
     (defvar yas-global-mode nil)
     (setq yas-triggers-in-field t
           yas-wrap-around-region t
           helm-yas-display-key-on-candidate t)
     ;; on multiple keys, fall back to completing read
     ;; typically this means helm
     (setq yas-prompt-functions '(yas-completing-prompt))
     ;; disable yas minor mode map
     ;; use hippie-expand instead
     (setq yas-minor-mode-map (make-sparse-keymap))
     ;; this makes it easy to get out of a nested expansion
     (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)

     (setq maple/yasnippets (expand-file-name "~/.emacs.d/yasnippets"))
     (if (and  (file-exists-p maple/yasnippets) (not (member maple/yasnippets yas-snippet-dirs)))
         (add-to-list 'yas-snippet-dirs maple/yasnippets))
     ))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'yas-minor-mode))

(eval-after-load 'web-mode
  '(progn
     (add-hook 'web-mode-hook #'yas-minor-mode)
     (add-hook 'html-mode-hook #'yas-minor-mode)
     (add-hook 'css-mode-hook #'yas-minor-mode)
     (add-hook 'js-mode-hook #'yas-minor-mode)))
(provide 'init-yasnippet)
