(require-package 'helm)
(require-package 'helm-projectile)
;; 必须的
(setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
; (require 'helm)
(require 'helm-config)
(require 'helm-projectile)

(setq helm-split-window-in-side-p           t
      helm-move-to-line-cycle-in-source     t
      helm-M-x-fuzzy-match                  t ;;模糊搜索
      helm-ff-search-library-in-sexp        t
      helm-buffers-fuzzy-matching           t
      helm-locate-fuzzy-match               t
      helm-recentf-fuzzy-match              t
      helm-echo-input-in-header-line        t
      helm-ff-file-name-history-use-recentf t)

; (define-key helm-command-map (kbd "o")     'helm-occur)
; (define-key helm-command-map (kbd "g")     'helm-do-grep)
; (define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)

; (require-package 'helm-ag)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)  ;;搜索子目录


; ; (substitute-key-definition 'find-tag 'helm-etags-select global-map)
; ; (setq projectile-completion-system 'helm)
; ; (helm-descbinds-mode)
(helm-mode 1)
; ; ;; enable Helm version of Projectile with replacment commands
; ; (helm-projectile-on)

(provide 'init-helm)
