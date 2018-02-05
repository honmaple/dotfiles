;; 必须的
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; helm里C-s搜索
;; C-c C-o分屏操作
;; 多文件C-x C-s
;; helm-color 比较有用的
(use-package helm
  :ensure t
  :defer t
  :diminish helm-mode
  :config
  (progn
    (setq helm-prevent-escaping-from-minibuffer t
          helm-allow-mouse t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t
          helm-echo-input-in-header-line t
          helm-imenu-execute-action-at-once-if-one nil
          helm-org-format-outline-path t
          helm-display-buffer-height 12
          helm-move-to-line-cycle-in-source     t
          helm-grep-save-buffer-name-no-confirm t)
    ;;模糊搜索
    (setq helm-M-x-fuzzy-match t
          helm-apropos-fuzzy-match t
          helm-file-cache-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-buffers-fuzzy-matching t)

    (defun maple/helm-hide-mode-line ()
      "Hide mode line in `helm-buffer'."
      (fset 'helm-display-mode-line #'ignore)
      (with-helm-buffer
        (setq-local mode-line-format nil)))

    (defun maple/helm-hide-cursor ()
      "Hide the cursor in helm buffers."
      (with-helm-buffer
        (setq cursor-in-non-selected-windows nil)))

    (defun maple/helm-hide-header-line ()
      "Hide the `helm' header if there is only one source."
      (if (> (length helm-sources) 1)
          (set-face-attribute
           'helm-source-header
           nil
           :foreground (face-attribute 'helm-source-header :foreground)
           :background (face-attribute 'helm-source-header :background)
           :box  (face-attribute 'helm-source-header :box)
           :height (face-attribute 'helm-source-header :height))
        (set-face-attribute
         'helm-source-header
         nil
         :foreground (face-attribute 'default :background)
         :background (face-attribute 'default :background)
         :box nil
         :height 0.1)))
    (add-hook 'helm-minibuffer-set-up-hook
              #'helm-hide-minibuffer-maybe)
    (add-hook 'helm-before-initialize-hook 'maple/helm-hide-header-line)
    (add-hook 'helm-after-initialize-hook 'maple/helm-hide-cursor)
    ;; (add-hook 'helm-after-initialize-hook 'maple/helm-hide-mode-line)
    ;; (defun maple/helm-hide-helper()
    ;;   (set-face-attribute 'helm-header nil
    ;;                       :foreground (face-attribute 'default :background)
    ;;                       :background (face-attribute 'default :background)
    ;;                       :box nil
    ;;                       :height 0.1))
    ;; (add-hook 'helm-before-initialize-hook 'maple/helm-hide-helper)
    (helm-mode 1)
    )
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("C-j" . helm-next-line)
         ("C-k" . helm-previous-line)
         ("C-h" . helm-next-source)
         ([escape] . helm-keyboard-quit)
         ("<tab>" . helm-execute-persistent-action)
         ("TAB" . helm-execute-persistent-action)
         :map helm-find-files-map
         ("C-h" . helm-find-files-up-one-level)
         :map helm-read-file-map
         ("C-h" . helm-find-files-up-one-level)
         ))

(use-package helm-ag
  :ensure t
  :defer t
  :init (advice-add 'helm-ag--edit :after #'evil-mc-mode)) ;;在helm-ag-edit中激活evil-mc

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode "ⓟ"
  :init
  (progn
    (setq projectile-sort-order 'recentf
          projectile-cache-file (concat maple-cache-directory
                                        "projectile.cache")
          projectile-known-projects-file (concat maple-cache-directory
                                                 "projectile-bookmarks.eld"))
    ;; (setq projectile-enable-caching t)
    ;; (add-to-list 'projectile-globally-ignored-files "*.png")
    ;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-hook 'after-init-hook #'projectile-global-mode))
  :config
  (progn
    (defun neotree-find-project-root ()
      (interactive)
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (let ((origin-buffer-file-name (buffer-file-name)))
          (neotree-find (projectile-project-root))
          (neotree-find origin-buffer-file-name)))))
  )

(use-package helm-projectile
  :ensure t
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project)
  :init
  (progn
    (setq projectile-switch-project-action 'helm-projectile)))


(provide 'init-helm)
