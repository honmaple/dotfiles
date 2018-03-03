(use-package esup)

(use-package docker-tramp)

(use-package dockerfile-mode)

(use-package quickrun
  :config
  (maple/set-quit-key quickrun--mode-map)
  (add-hook 'quickrun--mode-hook 'maple/truncate-lines))

(use-package blog-admin
  :load-path "site-lisp/blog-admin"
  :commands blog-admin-start
  :config
  (progn
    (setq blog-admin-backend-type 'pelican)
    (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
    (setq blog-admin-backend-new-post-with-same-name-dir nil) ;; create same-name directory with new post
    (setq blog-admin-backend-path "~/git/pelican")
    (setq blog-admin-backend-pelican-config-file "pelicanconf.py")
    (setq blog-admin-backend-pelican-posts-dir "content/org")
    (setq blog-admin-backend-pelican-org-mode-dir "content/org")
    (setq blog-admin-backend-pelican-markdown-dir "content/markdown")
    (setq blog-admin-backend-pelican-drafts-dir "content/draft")
    (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)

    (defun blog-set-face()
      "set face"
      (interactive)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family "WenQuanYi Micro Hei Mono" :size 18)))
      (defadvice switch-to-buffer (after switch-to-buffer activate)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family "DejaVu Sans Mono" :size 14)))))

    (when (display-graphic-p)
      (add-hook 'blog-admin-mode-hook 'font-lock-mode)
      (add-hook 'blog-admin-mode-hook 'blog-set-face))
    ))

(use-package imenu-list
  :commands imenu-list-minor-mode
  :evil-state
  (imenu-list-major-mode . emacs)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-mode-line-format "")
  (if (bound-and-true-p semantic-mode)
      (setq imenu-create-index-function 'semantic-create-imenu-index))
  :custom-face
  (imenu-list-entry-face-0 ((t (:foreground "#f92672"))))
  :bind (:map evil-leader--default-map
              ("bi" . imenu-list-minor-mode)
              :map imenu-list-major-mode-map
              ("d" . imenu-list-display-entry)
              ("q" . imenu-list-minor-mode)
              ("tb" . imenu-list-minor-mode)
              ("j" . next-line)
              ("k" . previous-line)))

(use-package youdao-dictionary
  :config
  (maple/set-quit-key youdao-dictionary-mode-map)
  (setq url-automatic-caching t
        youdao-dictionary-search-history-file (concat maple-cache-directory "youdao")
        youdao-dictionary-use-chinese-word-segmentation t))

;; (use-package cal-china-x
;;   :config
;;   (progn
;;     (setq mark-holidays-in-calendar t)
;;     (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
;;     (setq calendar-holidays cal-china-x-important-holidays)
;;     ))

(use-package avy
  :commands (maple/avy-open-url maple/avy-goto-url avy-pop-mark)
  :init
  (setq avy-all-windows 'all-frames)
  (setq avy-background t)
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

(use-package figlet)

(use-package 2048-game
  :evil-state
  (2048-mode . emacs))

(use-package maple-macro
  :demand t
  :load-path "site-lisp/maple"
  :config (maple/search-macro))

(use-package maple-startify
  :load-path "site-lisp/maple"
  :hook (emacs-startup . startify-mode))

(provide 'init-tool)
