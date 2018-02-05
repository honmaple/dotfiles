;; 必须的,使用频率排序
;; (use-package smex
;;   :ensure t)

(use-package ivy
  :ensure t
  :defer t
  :diminish ivy-mode
  :config
  (progn
    (ivy-mode)
    (with-eval-after-load 'recentf
      (setq ivy-use-virtual-buffers t))
    (setq ivy-height 12
          ivy-do-completion-in-region t
          ivy-wrap t
          ivy-extra-directories nil
          ivy-fixed-height-minibuffer t
          ;; Don't use ^ as initial input
          ivy-initial-inputs-alist nil
          ;; highlight til EOL
          ivy-format-function #'ivy-format-function-line
          ;; disable magic slash on non-match
          ;; ~ to /home/user
          ivy-magic-tilde nil
          ivy-use-virtual-buffers t
          ivy-virtual-abbreviate 'fullpath
          ivy-magic-slash-non-match-action nil)
    ;; (setq ivy-re-builders-alist
    ;;       '((t . ivy--regex-fuzzy)))
    ;; (setq confirm-nonexistent-file-or-buffer t)
    (setq ivy-re-builders-alist
          '((t   . ivy--regex-ignore-order)))
    (evil-make-overriding-map ivy-occur-mode-map 'normal)
    (setq completing-read-function 'ivy-completing-read
          read-file-name-function  'read-file-name-default)
    ))

(use-package counsel
  :ensure t
  :defer t
  :diminish counsel-mode
  :config
  (progn
    (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
      "Create parent directory if not exists while visiting file."
      (unless (file-exists-p filename)
        (let ((dir (file-name-directory filename)))
          (unless (file-exists-p dir)
            (if (y-or-n-p (format "directory %s does not exist,do you want you create it?" dir))
                (make-directory dir)
              (keyboard-quit))
            ))))
    (counsel-mode 1))
  :bind (("M-x" . counsel-M-x)
         ("C-x C-m" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("<tab>" . ivy-alt-done)
         ("TAB" . ivy-alt-done)
         ([escape] . minibuffer-keyboard-quit)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         ("C-<return>" . ivy-immediate-done)))


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
    (setq projectile-completion-system 'ivy)
    (defun neotree-find-project-root ()
      (interactive)
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (let ((origin-buffer-file-name (buffer-file-name)))
          (neotree-find (projectile-project-root))
          (neotree-find origin-buffer-file-name)))))
  )

;; (use-package counsel-projectile
;;   :ensure t
;;   :defer t
;;   :init (setq projectile-switch-project-action 'counsel-projectile-find-file))

(provide 'init-ivy)
