(require-package 'spaceline)
(require-package 'window-numbering)
(require-package 'smooth-scrolling)
;; (require-package 'centered-cursor-mode)
;; (require-package 'nlinum)
;; (require-package 'nlinum-relative)

;; (use-package nlinum
;;   :init (global-nlinum-mode t)
;;   :config (setq nlinum-format "%4d"))

;; (use-package nlinum-relative
;;   :config
;;   (nlinum-relative-setup-evil)
;;   (setq nlinum-relative-redisplay-delay 0)
;;   (add-hook 'prog-mode-hook 'nlinum-relative-mode))

(use-package smooth-scroll     ;; 光标位于中间
  :config (smooth-scrolling-mode 1)
  )

;; (use-package centered-cursor-mode     ;; 光标位于中间
;;   :config (global-centered-cursor-mode)
;;   )


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
