(use-package monokai-theme)


(use-package solarized-theme)

(use-package spacemacs-theme)

(use-package doom-themes)

(add-hook 'after-init-hook
          (lambda () (load-theme user-default-theme t)))


(use-package spaceline-config
  :ensure spaceline
  :commands (spaceline-spacemacs-theme)
  :init
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
  :config
  (progn
    (use-package maple-theme
      :demand t
      :load-path "site-lisp/maple"
      :config
      (defhydra maple/cycle-themes ()
        ("n" maple/next-theme "next theme")
        ("p" maple/previous-theme "prev theme")))
    ))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.2
        which-key-allow-evil-operators t)
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
  ;; :init
  ;; (setq nlinum-format "%3d")
  :hook ((prog-mode text-mode) . nlinum-mode))

;; (use-package nlinum-relative
;;   :commands (nlinum-relative-toggle nlinum-relative-on)
;;   :hook (nlinum-mode . nlinum-relative-on)
;;   :init
;;   (progn
;;     (setq nlinum-relative-current-symbol ""
;;           nlinum-relative-redisplay-delay 0)
;;     ;; (nlinum-relative-setup-evil)
;;     ))

;; ;;; 80列
(use-package fill-column-indicator
  :config
  (setq fci-rule-column 80)
  (setq fci-rule-width 1)
  (setq fci-rule-color "#D0BF8F")
  (push '(fci-mode "") minor-mode-alist))

;; (use-package vline
;;   :hook (after-init . vline-global-mode)
;;   :config
;;   ;; 与默认的行高亮的颜色相同
;;   (set-face-background vline-face "#3c3d37"))

;; 高亮括号
(use-package rainbow-delimiters
  :hook (after-init . rainbow-delimiters-mode)
  :config
  ;; 高亮括号配对
  (show-paren-mode 1)
  :diminish rainbow-delimiters-mode)

;; 颜色
(use-package rainbow-mode
  :hook ((prog-mode conf-unix-mode) . rainbow-mode)
  :diminish rainbow-mode)


;; 相同字符
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :hook
  ((prog-mode html-mode css-mode) . highlight-symbol-nav-mode)
  ((prog-mode html-mode css-mode) . highlight-symbol-mode)
  ((org-mode) . highlight-symbol-nav-mode))


(use-package volatile-highlights
  :hook (after-init . volatile-highlights-mode)
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
  :diminish highlight-parentheses-mode
  :hook (after-init . global-highlight-parentheses-mode)
  :config
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"))
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; (use-package col-highlight
;;   :defer t
;;   :hook (after-init . column-highlight-mode)
;;   :config (set-face-background col-highlight-face "#3c3d37"))

;; 显示缩进
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))


(use-package whitespace
  :ensure nil
  :diminish whitespace-mode "ⓦ"
  :hook ((prog-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face
                           trailing space-before-tab
                           indentation empty space-after-tab))
  (setq whitespace-action '(auto-cleanup)))

(provide 'init-ui)
