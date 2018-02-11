(use-package monokai-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package spacemacs-theme
  :ensure t
  :defer t)

(use-package doom-themes
  :ensure t
  :defer t)

(add-hook 'after-init-hook
          (lambda () (load-theme user-default-theme t)))


(use-package spaceline-config
  :ensure spaceline
  :config
  (progn
    (defun maple/set-spaceline()
      "spaceline config"
      ;; (set-face-attribute 'mode-line nil :box nil)
      (setq powerline-default-separator 'wave
            spaceline-workspace-numbers-unicode nil
            spaceline-window-numbers-unicode t
            spaceline-highlight-face-func 'spaceline-highlight-face-evil-state
            spaceline-helm-help-p nil)
      (spaceline-spacemacs-theme)
      (when (package-installed-p 'helm)
        (spaceline-helm-mode t))
      (maple/set-powerline-for-startup-buffers))
    (defun maple/set-powerline-for-startup-buffers ()
      "Set the powerline for buffers created when Emacs starts."
      (dolist (buffer '("*Messages*" "*Compile-Log*"))
        (when (and (get-buffer buffer)
                   (with-current-buffer buffer
                     (setq-local mode-line-format (default-value 'mode-line-format))
                     (powerline-set-selected-window)
                     (powerline-reset))))))
    (add-hook 'after-init-hook 'maple/set-spaceline)
    ))

(use-package hydra
  :ensure t
  :defer t
  :config
  (progn
    (use-package maple-theme
      :load-path "site-lisp/maple"
      :config
      (defhydra maple/cycle-themes ()
        ("n" maple/next-theme "next theme")
        ("p" maple/previous-theme "prev theme")))
    ))


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :defer t
  :init
  (setq which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.2
        which-key-allow-evil-operators t)
  (add-hook 'after-init-hook #'which-key-mode)
  :config
  (which-key-add-key-based-replacements
    ",f" "file"
    ",b" "buffer"
    ",o" "orgmode"
    ",e" "flycheck error"
    ",j" "avy"
    ",g" "git"
    ",w" "window"
    ",p" "project"
    ",sq" "sql"
    ",t" "toggle mode"))

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
  (setq fci-rule-column 80)
  (setq fci-rule-width 1)
  (setq fci-rule-color "#D0BF8F")
  (push '(fci-mode "") minor-mode-alist))

;; (use-package vline
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'after-init-hook 'vline-global-mode)
;;   :config
;;   ;; 与默认的行高亮的颜色相同
;;   (set-face-background vline-face "#3c3d37"))

;; 高亮括号
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  ;; 高亮括号配对
  (show-paren-mode 1)
  :diminish rainbow-delimiters-mode)

;; 颜色
(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode)
  (add-hook 'conf-unix-mode-hook 'rainbow-mode)
  :diminish rainbow-mode)


;; 相同字符
(use-package highlight-symbol
  :ensure t
  :defer t
  :diminish highlight-symbol-mode
  :init
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
  (add-hook 'org-mode-hook 'highlight-symbol-nav-mode))


(use-package volatile-highlights
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook #'volatile-highlights-mode)
  :diminish volatile-highlights-mode
  :config
  (progn
    ;; additional extensions
    ;; evil
    (after-load 'evil
      (vhl/define-extension 'evil
                            'evil-move
                            'evil-paste-after
                            'evil-paste-before
                            'evil-paste-pop)
      (vhl/install-extension 'evil))
    ;; undo-tree
    (after-load 'undo-tree
      (vhl/define-extension 'undo-tree
                            'undo-tree-move
                            'undo-tree-yank)
      (vhl/install-extension 'undo-tree)))
  :custom-face (vhl/default-face ((t (:background "Springgreen3" :foreground "#272822")))))

;; 高亮当前括号
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :init (add-hook 'after-init-hook #'global-highlight-parentheses-mode)
  :config
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"))
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; (use-package col-highlight
;;   :defer t
;;   :init (add-hook 'after-init-hook #'column-highlight-mode)
;;   :config (set-face-background col-highlight-face "#3c3d37"))

;; 显示缩进
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))


(use-package whitespace
  :defer t
  :diminish whitespace-mode "ⓦ"
  :init
  (dolist (hook '(prog-mode-hook
                  conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))
  (setq whitespace-action '(auto-cleanup)))

(provide 'init-ui)
