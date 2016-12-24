(require-package 'spaceline)
(require-package 'window-numbering)
(require-package 'smooth-scrolling)
(require-package 'which-key)
(require-package 'rainbow-delimiters)  ;;括号高亮
(require-package 'rainbow-mode)
(require-package 'undo-tree)
(require-package 'highlight-symbol)

(when (< emacs-major-version 24)
  (require-package 'color-theme))

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'monokai-theme)

(use-package monokai-theme
  :defer t
  :init (load-theme 'monokai t)
  )


(require-package 'fill-column-indicator)

;;; 80列
(use-package fill-column-indicator
  :defer t
  :init
  (progn
    (setq fci-rule-column 80)
    (setq fci-rule-width 1)
    (setq fci-rule-color "#D0BF8F")
    (push '(fci-mode "") minor-mode-alist)))


;; (require-package 'linum-relative)
;; (use-package linum-relative ;;相对行号
;;   :commands (linum-relative-toggle linum-relative-on)
;;   :init
;;   (progn
;;     (global-linum-mode t)
;;     (linum-relative-on))
;;   :config (setq linum-relative-current-symbol ""))

(use-package smooth-scroll     ;; 光标位于中间
  :config (smooth-scrolling-mode 1))

(use-package window-numbering
  :defer t
  :init (window-numbering-mode)) ;;这个要在前

(use-package spaceline-config
  :config
  (progn
    (require 'spaceline-config)
    (set-face-attribute 'mode-line nil :box nil)
    (setq ns-use-srgb-colorspace nil)
    (setq powerline-default-separator 'wave)
    (setq spaceline-toggle-window-number-on-p t)
    (setq spaceline-toggle-workspace-number-on-p nil)
    (setq spaceline-workspace-numbers-unicode t)
    (setq spaceline-window-numbers-unicode t)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-spacemacs-theme)
    (spaceline-helm-mode t)
    ))

(use-package which-key
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


;; 高亮括号
(use-package rainbow-delimiters
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; 颜色
(use-package rainbow-mode
  :defer t
  :init (add-hook 'prog-mode-hook 'rainbow-mode)
  :diminish rainbow-mode)

(use-package undo-tree
  :defer t
  :init
  (progn
    (setq undo-tree-history-directory-alist
          `(("." . ,(concat maple-cache-directory "undo-tree"))))
    ;; (unless (file-exists-p (concat maple-cache-directory "undo-tree"))
    ;;   (make-directory (concat maple-cache-directory "undo-tree")))
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-auto-save-history t)
    (global-undo-tree-mode))
  :diminish undo-tree-mode)

(use-package highlight-symbol
  :defer t
  :diminish highlight-symbol-mode
  :init
  (progn
    (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
      (add-hook hook 'highlight-symbol-mode)
      (add-hook hook 'highlight-symbol-nav-mode))
    (add-hook 'org-mode-hook 'highlight-symbol-nav-mode)))

(use-package highlight-indentation
  :defer t
  :diminish highlight-indentation-mode)

(provide 'init-ui)
