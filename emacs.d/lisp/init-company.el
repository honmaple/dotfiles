(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode)
  :diminish yas-minor-mode "ⓨ"
  :init
  (progn
    (defvar yas-global-mode nil)
    (setq yas-triggers-in-field t
          yas-wrap-around-region t
          yas-prompt-functions '(yas-completing-prompt)
          yas-minor-mode-map (make-sparse-keymap))
    ;; helm-yas-display-key-on-candidate t)

    (use-package yasnippet-snippets
      :ensure t
      :defer t)

    (defun maple/load-yasnippet ()
      (unless yas-global-mode (yas-global-mode 1))
      (yas-minor-mode 1))
    (add-hook 'prog-mode-hook 'maple/load-yasnippet)
    )
  :bind (:map yas-minor-mode-map
              ("M-s-/" . yas-next-field)))

(use-package company
  :ensure t
  :defer t
  :diminish company-mode " ⓐ"
  :init
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-tooltip-limit 15
        company-minimum-prefix-length 1
        company-dabbrev-downcase t  ;;忽略大小写
        company-dabbrev-ignore-case t
        ;; company-dabbrev-other-buffers t
        company-begin-commands '(self-insert-command)
        company-global-modes '(not comint-mode erc-mode gud-mode rcirc-mode
                                   minibuffer-inactive-mode inferior-python-mode shell-mode evil-command-window-mode))
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-backends
        '((company-dabbrev-code company-gtags company-etags company-capf company-keywords company-files)
          ;; (company-semantic)
          (company-dabbrev)
          ))

  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  :custom-face
  (company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  :bind (:map company-active-map
              ("C-/" . company-search-candidates)
              ("C-d" . company-show-doc-buffer)
              ("C-M-/" . company-filter-candidates)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("<RET>" . company-complete-selection)))



(use-package company-statistics
  :ensure t
  :after company
  :config
  (setq company-statistics-file (concat maple-cache-directory
                                        "company-statistics-cache.el"))
  (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-quickhelp
  :ensure t
  :if (display-graphic-p)
  :commands company-quickhelp-manual-begin
  :after company
  :config
  (setq company-quickhelp-delay 1)
  (defun maple/modify-help-pos-tip ()
    "Modify company-quickhelp."
    (setq pos-tip-foreground-color "#00ffff"
          pos-tip-background-color "#272822"
          pos-tip-border-width 0))
  (add-hook 'company-quickhelp-mode-hook 'maple/modify-help-pos-tip))

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

;; (use-package ycmd
;;   :defer t
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook 'ycmd-mode)
;;     (set-variable 'ycmd-server-command '("python2" "/home/jianglin/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"))
;;     ;; (set-variable 'ycmd-global-config "/home/jianglin/.emacs.d/layer/+tool/ycmd/global_config.py")
;;     (set-variable 'ycmd-global-config "/home/jianglin/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py")
;;     ))

;; (use-package company-ycmd
;;   :defer t
;;   :commands company-ycmd)

(provide 'init-company)
