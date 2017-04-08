(require-package 'company)
(require-package 'company-statistics)
(require-package 'company-quickhelp)
(require-package 'yasnippet)

(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode)
  :diminish yas-minor-mode "ⓨ"
  :init
  (progn
    ;;   ;; (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode))
  :config
  (progn
    (defvar yas-global-mode nil)
    (setq yas-triggers-in-field t
          yas-wrap-around-region t
          helm-yas-display-key-on-candidate t)
    (setq yas-prompt-functions '(yas-completing-prompt))
    (setq yas-minor-mode-map (make-sparse-keymap))
    (setq maple/yasnippets (expand-file-name "~/.emacs.d/yasnippets"))
    (if (and  (file-exists-p maple/yasnippets) (not (member maple/yasnippets yas-snippet-dirs)))
        (add-to-list 'yas-snippet-dirs maple/yasnippets))
    (defun maple/load-yasnippet ()
      (unless yas-global-mode (yas-global-mode 1))
      (yas-minor-mode 1))
    (add-hook 'prog-mode-hook 'maple/load-yasnippet)
    )
  :bind (:map yas-minor-mode-map
              ("M-s-/" . yas-next-field)))


(use-package hippie-exp
  :defer t
  :config
  (progn
    ;; (global-set-key (kbd "M-/") 'hippie-expand)
    (setq hippie-expand-try-functions-list
          '(try-complete-file-name-partially
            try-complete-file-name
            try-expand-dabbrev
            try-expand-dabbrev-all-buffers
            try-expand-dabbrev-from-kill))
    )
  :bind (:map evil-insert-state-map
              ("<backtab>" . hippie-expand)))


(use-package company
  :defer t
  :diminish company-mode " ⓐ"
  :init
  (progn
    (setq ;; company-echo-delay 0
     company-idle-delay 0.1
     company-show-numbers t
     company-tooltip-limit 15
     company-minimum-prefix-length 1
     company-dabbrev-downcase t  ;;忽略大小写
     company-dabbrev-ignore-case t
     ;; company-dabbrev-other-buffers t
     company-begin-commands '(self-insert-command)
     company-global-modes '(not comint-mode erc-mode gud-mode rcirc-mode
                                minibuffer-inactive-mode inferior-python-mode shell-mode))
    (defvar-local company-fci-mode-on-p nil)
    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
    (add-hook 'after-init-hook 'global-company-mode)
    )
  :config
  (progn
    (setq company-backends
          '((company-dabbrev-code company-gtags company-etags company-capf company-keywords company-files)
            ;; (company-semantic)
            (company-dabbrev)
            ))

    (custom-set-faces
     '(company-tooltip-common
       ((t (:inherit company-tooltip :weight bold :underline nil))))
     '(company-tooltip-common-selection
       ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

    )
  :bind (:map company-active-map
              ("C-/" . company-search-candidates)
              ("C-M-/" . company-filter-candidates)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("<RET>" . company-complete-selection)))



(use-package company-statistics
  :defer t
  :init
  (progn
    (defun maple/set-company-statistics()
      "company-statistics config"
      (setq company-statistics-file (concat maple-cache-directory
                                            "company-statistics-cache.el"))
      (add-hook 'company-mode-hook 'company-statistics-mode)))
  (add-hook 'after-init-hook 'maple/set-company-statistics))

(use-package company-quickhelp
  :if (and t (display-graphic-p))
  :commands company-quickhelp-manual-begin
  :init
  (progn
    ;; (add-hook 'company-mode-hook 'company-quickhelp-mode)
    (after-load 'company
      (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))))
  :config
  (progn
    (setq company-quickhelp-delay 1)
    (defun maple/modify-help-pos-tip ()
      "Modify company-quickhelp."
      (setq pos-tip-foreground-color "#00ffff"
            pos-tip-background-color "#272822"
            pos-tip-border-width 0))
    (add-hook 'company-quickhelp-mode-hook 'maple/modify-help-pos-tip)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)

(defun maple/add-company-backend (backend)
  "Add BACKEND to `company-backends'."
  (after-load 'company
    (setq-local company-backends (append (list backend) company-backends))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(defun maple/add-to-company-backend (backend &optional hook)
  "Add BACKEND to `company-backends'."
  (lexical-let ((backend backend)
                (hook hook))
    (if hook
        (add-hook hook (lambda () (maple/add-company-backend backend)))
      (lambda () (maple/add-company-backend backend)))))

(provide 'init-company)
