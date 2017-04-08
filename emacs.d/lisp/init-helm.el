(require-package 'helm)
(require-package 'helm-ag)
(require-package 'projectile)
(require-package 'helm-projectile)


;; 必须的
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; helm里C-s搜索
;; C-c C-o分屏操作
;; 多文件C-x C-s
;; helm-color 比较有用的
(use-package helm
  :defer t
  :diminish helm-mode
  :config
  (progn
    (setq helm-prevent-escaping-from-minibuffer t
          helm-bookmark-show-location t
          helm-display-header-line nil
          helm-split-window-in-side-p t
          helm-always-two-windows t
          helm-echo-input-in-header-line t
          helm-imenu-execute-action-at-once-if-one nil
          helm-org-format-outline-path t
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
    (defun maple/hide-cursor-in-helm-buffer ()
      "Hide the cursor in helm buffers."
      (with-helm-buffer
        (setq cursor-in-non-selected-windows nil)))
    (add-hook 'helm-after-initialize-hook 'maple/hide-cursor-in-helm-buffer)
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
  :defer t
  :init (advice-add 'helm-ag--edit :after #'evil-mc-mode)) ;;在helm-ag-edit中激活evil-mc

(use-package projectile
  :defer t
  :diminish projectile-mode "ⓟ"
  :init
  (progn
    (setq projectile-enable-caching t)
    ;; (add-to-list 'projectile-globally-ignored-files "*.png")
    ;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-hook 'after-init-hook #'projectile-global-mode))
  )

(use-package helm-projectile
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

;; (require-package 'helm-swoop)
;; (use-package helm-swoop
;;   :defer t
;;   :init
;;   (progn
;;     (setq helm-swoop-split-with-multiple-windows t
;;           helm-swoop-split-direction 'split-window-vertically
;;           helm-swoop-speed-or-color t
;;           helm-swoop-split-window-function 'helm-default-display-buffer
;;           helm-swoop-pre-input-function (lambda () ""))
;;     (advice-add 'helm-swoop--edit :after #'evil-mc-mode)) ;;在helm-swoop-edit中激活evil-mc
;;   :bind (:map  helm-swoop-edit-map
;;                ("C-c C-c" . helm-swoop--edit-complete)
;;                ("C-c C-k" . helm-swoop--edit-cancel))
;;   )
(provide 'init-helm)
