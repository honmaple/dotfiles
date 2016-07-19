(require-package 'spaceline)
(require-package 'window-numbering)
(require-package 'smooth-scrolling)

(use-package smooth-scroll     ;; 光标位于中间
  :init (smooth-scrolling-mode 1)
  )

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
    ))

(provide 'init-modeline)
