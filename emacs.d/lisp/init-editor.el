;; 注释
(use-package adaptive-wrap
  :hook (visual-line-mode adaptive-wrap-prefix-mode))

;; 修改外部文件自动载入
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :diminish auto-revert-mode
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; (use-package semantic
;;   :init
;;   (setq srecode-map-save-file
;;         (concat maple-cache-directory "srecode-map.el"))
;;   (setq semanticdb-default-save-directory
;;         (concat maple-cache-directory "semanticdb/"))
;;   ;; (add-hook 'after-init-hook 'semantic-mode)
;;   :config
;;   (progn
;;     (add-to-list 'semantic-default-submodes
;;                  'global-semantic-stickyfunc-mode)
;;     (add-to-list 'semantic-default-submodes
;;                  'global-semantic-idle-summary-mode)))


(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :config
  ;; (setq electric-pair-pairs '((?\' . ?\')))
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; (use-package which-func
;;   :hook (after-init which-function-mode))

(use-package dumb-jump
  :after (evil)
  :config
  (setq dumb-jump-selector 'helm)
  :evil-bind
  (normal prog-mode-map
          "gd"  'dumb-jump-go))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook
  ;; enable eldoc in `eval-expression'
  (eval-expression-minibuffer-setup . eldoc-mode)
  ;; enable eldoc in IELM
  (ielm-mode . eldoc-mode))


(use-package comint
  :ensure nil
  :evil-state
  (comint-mode . insert)
  :config
  (setq comint-prompt-read-only t)
  :bind
  (:map comint-mode-map
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)
        ;; ("<mouse-4>" . comint-previous-input)
        ;; ("<mouse-5>" . comint-next-input)
        ("<escape>" . (lambda() (interactive)
                        (goto-char (cdr comint-last-prompt))))
        ))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook ((yaml-mode conf-mode prog-mode) . hs-minor-mode))


(use-package projectile
  :diminish projectile-mode "ⓟ"
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-sort-order 'recentf
        projectile-cache-file (concat maple-cache-directory
                                      "projectile.cache")
        projectile-known-projects-file (concat maple-cache-directory
                                               "projectile-bookmarks.eld"))
  :config
  (defun neotree-find-project-root ()
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (let ((origin-buffer-file-name (buffer-file-name)))
        (neotree-find (projectile-project-root))
        (neotree-find origin-buffer-file-name))))
  )


(provide 'init-editor)
