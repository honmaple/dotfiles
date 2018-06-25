(use-package esup)
(use-package ctable)
(use-package docker-tramp)
(use-package dockerfile-mode)

(use-package maple-minimap
  :commands (minimap-mode)
  :load-path "site-lisp/maple"
  :evil-state (minimap-mode . emacs))

(use-package pangu-spacing
  :commands (pangu-spacing-space-current-buffer))

(use-package quickrun
  :hook (quickrun--mode . maple/truncate-lines)
  :config
  (maple/set-quit-key quickrun--mode-map))

(use-package blog-admin
  :load-path "site-lisp/blog-admin"
  :commands blog-admin-start
  :config
  (setq blog-admin-backend-type 'pelican
        blog-admin-backend-new-post-in-drafts t ;; create new post in drafts by default
        blog-admin-backend-new-post-with-same-name-dir nil ;; create same-name directory with new post
        blog-admin-backend-path "~/git/pelican"
        blog-admin-backend-pelican-config-file "pelicanconf.py"
        blog-admin-backend-pelican-posts-dir "content/org"
        blog-admin-backend-pelican-org-mode-dir "content/org"
        blog-admin-backend-pelican-markdown-dir "content/markdown"
        blog-admin-backend-pelican-drafts-dir "content/draft")
  (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)

  (defun blog-set-face()
    "set face"
    (interactive)
    (set-face-attribute 'variable-pitch nil :font "-Sony-Sony Fixed-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (buffer-face-mode))

  (when (display-graphic-p)
    (add-hook 'blog-admin-mode-hook 'blog-set-face)))

(use-package imenu-list
  :commands imenu-list-minor-mode
  :evil-state
  (imenu-list-major-mode . emacs)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-mode-line-format mode-line-format)
  (when (bound-and-true-p semantic-mode)
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
  :commands (avy-pop-mark)
  :config
  (setq avy-all-windows 'all-frames
        avy-background t))

(use-package figlet)

(use-package 2048-game
  :evil-state
  (2048-mode . emacs))

(use-package maple-macro
  :load-path "site-lisp/maple"
  :hook (after-init . maple/search-init))

(use-package maple-startify
  :load-path "site-lisp/maple"
  :hook (emacs-startup . startify-mode))

(provide 'init-tool)
