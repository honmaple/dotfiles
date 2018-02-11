;; 注释
(use-package adaptive-wrap
  :ensure t
  :defer t
  :config
  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))

;; 修改外部文件自动载入
(use-package autorevert
  :defer t
  :init (add-hook 'after-init-hook #'global-auto-revert-mode)
  :diminish auto-revert-mode
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package semantic
  :ensure t
  :defer t
  :init
  (setq srecode-map-save-file
        (concat maple-cache-directory "srecode-map.el"))
  (setq semanticdb-default-save-directory
        (concat maple-cache-directory "semanticdb/"))
  ;; (add-hook 'after-init-hook 'semantic-mode)
  :config
  (progn
    (add-to-list 'semantic-default-submodes
                 'global-semantic-stickyfunc-mode)
    (add-to-list 'semantic-default-submodes
                 'global-semantic-idle-summary-mode)))

(use-package stickyfunc-enhance
  :ensure t
  :defer t)


(use-package elec-pair
  :defer t
  :init (add-hook 'after-init-hook #'electric-pair-mode)
  :config
  ;; (setq electric-pair-pairs '((?\' . ?\')))
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; (use-package page
;;   :init
;;   (progn
;;     ;; Don't disable narrowing commands
;;     (put 'narrow-to-region 'disabled nil)
;;     (put 'narrow-to-page 'disabled nil)
;;     (put 'narrow-to-defun 'disabled nil)
;;     ))

;; (use-package which-func
;;   :init (add-hook 'after-init-hook 'which-function-mode))

(use-package dumb-jump
  :ensure t
  :defer t
  :config
  (setq dumb-jump-selector 'helm)
  :bind
  (:map evil-normal-state-map
        ("gd" . dumb-jump-go)))


(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :config
  ;; enable eldoc in `eval-expression'
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  ;; enable eldoc in IELM
  (add-hook 'ielm-mode-hook #'eldoc-mode))


(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  :init
  (add-hook 'yaml-mode-hook #'hs-minor-mode)
  (add-hook 'conf-mode-hook #'hs-minor-mode)
  (add-hook 'prog-mode-hook #'hs-minor-mode))

(provide 'init-editor)
