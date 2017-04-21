(use-package monokai-theme
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (lambda () (load-theme 'monokai t)))
  ;; :init (load-theme 'monokai t)
  )

;; (use-package color-theme-approximate
;;   :ensure t
;;   :init (color-theme-approximate-on))

(use-package window-numbering
  :ensure t
  :defer t
  ;; :init (add-hook 'after-init-hook #'window-numbering-mode));;这个要在前
  :init (window-numbering-mode));;这个要在前

(use-package spaceline-config
  :ensure spaceline
  :config
  (progn
    (defun maple/set-spaceline()
      "spaceline config"
      (set-face-attribute 'mode-line nil :box nil)
      (setq ns-use-srgb-colorspace nil)
      (setq powerline-default-separator 'wave)
      (setq spaceline-toggle-window-number-on-p t)
      (setq spaceline-toggle-workspace-number-on-p nil)
      (setq spaceline-workspace-numbers-unicode t)
      (setq spaceline-window-numbers-unicode t)
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
      (spaceline-spacemacs-theme)
      (when (package-installed-p 'helm)
        (spaceline-helm-mode t)))
    (add-hook 'after-init-hook 'maple/set-spaceline)
    ))

(use-package which-key
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook #'which-key-mode)
  :diminish which-key-mode
  :config
  (progn
    (which-key-setup-side-window-bottom)
    (setq which-key-special-keys nil
          which-key-use-C-h-for-paging t
          which-key-prevent-C-h-from-cycling t
          which-key-echo-keystrokes 0.02
          which-key-max-description-length 32
          which-key-sort-order 'which-key-key-order-alpha
          which-key-idle-delay 0.2
          which-key-allow-evil-operators t)
    ))

(use-package nlinum
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'nlinum-mode)
    (add-hook 'text-mode-hook 'nlinum-mode)
    ;; (setq nlinum-format "%3d")
    ))

;; (use-package nlinum-relative
;;   :ensure t
;;   :commands (nlinum-relative-toggle nlinum-relative-on)
;;   :init
;;   (progn
;;     (setq nlinum-relative-current-symbol ""
;;           nlinum-relative-redisplay-delay 0)
;;     ;; (nlinum-relative-setup-evil)
;;     (add-hook 'nlinum-mode-hook 'nlinum-relative-on)
;;     ))

;; ;;; 80列
(use-package fill-column-indicator
  :ensure t
  :defer t
  :config
  (progn
    ;; (setq fci-rule-column 80)
    (setq fci-rule-width 1)
    (setq fci-rule-color "#D0BF8F")
    (push '(fci-mode "") minor-mode-alist)))

;; 高亮括号
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  (progn
    ;; 高亮括号配对
    (show-paren-mode 1))
  :diminish rainbow-delimiters-mode)

;; 颜色
(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-mode)
    (add-hook 'conf-unix-mode-hook 'rainbow-mode))
  :diminish rainbow-mode)


;; 相同字符
(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish highlight-symbol-mode
  :init
  (progn
    (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
      (add-hook hook 'highlight-symbol-mode)
      (add-hook hook 'highlight-symbol-nav-mode))
    (add-hook 'org-mode-hook 'highlight-symbol-nav-mode)))

;; 显示缩进
;; (use-package highlight-indentation
;;   :defer t
;;   :diminish highlight-indentation-mode
;;   :init (add-hook 'prog-mode-hook 'highlight-indentation-mode))

;; 光标位于中间 ;; emacs已内置
;; (use-package smooth-scrolling
;;   :defer t
;;   :init (add-hook 'after-init-hook #'smooth-scrolling-mode)
;;   :config
;;   (progn
;;     (setq scroll-preserve-screen-position t
;;           scroll-margin 0
;;           scroll-conservatively 101)
;;     ))




(provide 'init-ui)
