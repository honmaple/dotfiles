(use-package monokai-theme
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook (lambda () (load-theme 'monokai t))))

(use-package spacemacs-theme
  :ensure t
  :defer t)
;; :init (add-hook 'after-init-hook (lambda () (load-theme 'spacemacs-dark t))))

(use-package doom-themes
  :ensure t
  :defer t)
;; :init (add-hook 'after-init-hook (lambda () (load-theme 'doom-monokai t))))

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
      (setq spaceline-workspace-numbers-unicode nil)
      (setq spaceline-window-numbers-unicode t)
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
      (spaceline-spacemacs-theme)
      ;; (spaceline-compile)
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
  :defer t
  :ensure t
  :config
  (progn
    ;; (setq maple-cycle-themes (mapcar 'symbol-name (custom-available-themes)))
    (setq maple-cycle-themes (delete "doom-one-light"
                                     (mapcar 'symbol-name (custom-available-themes))))
    (defun maple/cycle-theme (num)
      (interactive)
      (setq maple-current-theme-index
            (+ num
               (cl-position
                (car (mapcar 'symbol-name custom-enabled-themes)) maple-cycle-themes :test 'equal)))
      (when (>= maple-current-theme-index (length maple-cycle-themes))
        (setq maple-current-theme-index 0))
      (setq maple-current-theme (nth maple-current-theme-index maple-cycle-themes))
      (mapc 'disable-theme custom-enabled-themes)
      (let ((progress-reporter
             (make-progress-reporter
              (format "Loading theme %s..." maple-current-theme))))
        (load-theme (intern maple-current-theme) t)
        (progress-reporter-done progress-reporter)))
    (defun maple/next-theme()
      (interactive)
      (maple/cycle-theme 1))
    (defun maple/previous-theme()
      (interactive)
      (maple/cycle-theme -1))
    (defhydra maple/cycle-themes ()
      ("n" maple/next-theme "next theme")
      ("p" maple/previous-theme "prev theme"))
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
      ",t" "toggle mode")
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
  (progn
    ;; 高亮括号配对
    (show-paren-mode 1)
    )
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
