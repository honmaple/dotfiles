(require-package 'youdao-dictionary)
(require-package 'avy)
(require-package 'restclient)
(require-package 'quickrun)

(use-package esup)

(use-package quickrun
  :defer t
  :config (evil-set-initial-state 'quickrun/mode 'emacs)
  )

(use-package epa
  :config
  (progn
    (auto-encryption-mode -1)
    ))

(use-package imenu-list
  :defer t
  :init
  (progn
    (defun maple/imenu-mode ()
      (define-key evil-normal-state-map (kbd "tb") 'imenu-list-minor-mode))
    (add-hook 'prog-mode-hook 'maple/imenu-mode)
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize nil))
  :config
  (progn
    (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*Ilist\\*")
    (after-load "imenu-list"
      (define-key imenu-list-major-mode-map (kbd "j") 'next-line)
      (define-key imenu-list-major-mode-map (kbd "k") 'previous-line))
    )
  :bind (:map evil-leader--default-map
              ("bi" . imenu-list-minor-mode)
              :map imenu-list-major-mode-map
              ("d" . imenu-list-display-entry)
              ("q" . imenu-list-minor-mode)))

(use-package youdao-dictionary
  :defer t
  :config
  (progn
    ;; (evil-set-initial-state 'youdao-dictionary-mode 'emacs)
    (setq url-automatic-caching t
          ;; Set file path for saving search history
          youdao-dictionary-search-history-file (concat maple-cache-directory "youdao")
          ;; Enable Chinese word segmentation support
          youdao-dictionary-use-chinese-word-segmentation t))
  )
;; (use-package cal-china-x
;;   :config
;;   (progn
;;     (setq mark-holidays-in-calendar t)
;;     (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
;;     (setq calendar-holidays cal-china-x-important-holidays)
;;     ))

(use-package avy
  :defer t
  :commands (maple/avy-open-url maple/avy-goto-url avy-pop-mark)
  :init
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t))
  :config
  (progn
    (defun maple/avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy--generic-jump "https?://" nil 'pre))
    (defun maple/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (maple/avy-goto-url)
        (browse-url-at-point)))))

(use-package restclient
  :defer t
  :bind (:map evil-leader--default-map
              ("rs" . restclient-http-send-current)
              ("rr" . restclient-http-send-current-raw)
              ("rn" . restclient-narrow-to-current)
              ("ry" . restclient-copy-curl-command)))

(use-package 2048-game
  :ensure t
  :config
  (progn
    (evil-set-initial-state '2048-mode 'emacs)
    ))

(provide 'init-tool)
