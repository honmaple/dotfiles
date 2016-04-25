(require-package 'helm)
(require-package 'helm-ag)
(require-package 'helm-projectile)
; (require-package 'helm-ag)
;; 必须的
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

; (require 'helm-config)
(require 'helm-config)
; (require 'helm-projectile)

(eval-after-load "helm"
                 (progn
                   (setq helm-prevent-escaping-from-minibuffer t
                         helm-bookmark-show-location t
                         helm-display-header-line nil
                         helm-split-window-in-side-p t
                         helm-always-two-windows t
                         helm-echo-input-in-header-line t
                         helm-imenu-execute-action-at-once-if-one nil
                         helm-org-format-outline-path t
                         helm-move-to-line-cycle-in-source     t)
                   ;; helm-ff-search-library-in-sexp        t
                   ;; helm-buffers-fuzzy-matching           t
                   ;; helm-locate-fuzzy-match               t
                   ;; helm-recentf-fuzzy-match              t
                   ;; helm-echo-input-in-header-line        t
                   ;; helm-lisp-fuzzy-completion            t
                   ;; helm-ff-file-name-history-use-recentf t)
                   (setq helm-M-x-fuzzy-match t  ;;模糊搜索
                         helm-apropos-fuzzy-match t
                         helm-file-cache-fuzzy-match t
                         helm-imenu-fuzzy-match t
                         helm-lisp-fuzzy-completion t
                         helm-recentf-fuzzy-match t
                         helm-semantic-fuzzy-match t
                         helm-buffers-fuzzy-matching t)))



(with-eval-after-load 'helm
                      (define-key helm-map (kbd "C-j") 'helm-next-line)
                      (define-key helm-map (kbd "C-k") 'helm-previous-line)
                      (define-key helm-map (kbd "C-h") 'helm-next-source)
                      (define-key helm-map [escape]    'helm-keyboard-quit)
                      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
                      ; ; (define-key helm-find-files-map (kbd "S-<tab>") 'helm-find-files-up-one-level)
                      ; ; (define-key helm-find-files-map (kbd "<backtab>") 'helm-find-files-up-one-level)
                      (setq helm-grep-save-buffer-name-no-confirm t))

;; helm里C-s搜索
;; C-c C-o分屏操作
;; 多文件C-x C-s

;; helm-color 比较有用的
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ;; 粘贴板选择
; (global-set-key (kbd "C-x b") 'helm-mini)         ;;显示缓冲区(已经打开的文件)

(helm-mode 1)
(add-hook 'after-init-hook #'projectile-global-mode)
(with-eval-after-load 'projectile
  (helm-projectile-on)
  (setq projectile-enable-caching t)
  )

(provide 'init-helm)
