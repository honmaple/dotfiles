(require-package 'company)
(require-package 'company-statistics)
(require-package 'company-quickhelp)


;; (add-hook 'c-mode-hook 'ycmd-mode)
;; (add-hook 'python-mode-hook 'ycmd-mode)
;; (add-hook 'web-mode-hook 'ycmd-mode)
;; (add-hook 'after-init-hook #'global-ycmd-mode)
;; ;; (company-ycmd-setup)
;; ;; (flycheck-ycmd-setup)
;; (set-variable 'ycmd-server-command '("python2" "/home/jianglin/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd"))
;; ;; (set-variable 'ycmd-global-config "/home/jianglin/.emacs.d/layer/+tool/ycmd/global_config.py")
;; (set-variable 'ycmd-global-config "/home/jianglin/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py")
;; (require 'company-ycmd)
;; (company-ycmd-setup)

(use-package company
  :defer t
  :diminish company-mode " ⓐ"
  :init
  (progn
    (setq ;; company-echo-delay 0
     company-idle-delay 0.1
     ;; company-auto-complete nil
     company-show-numbers t
     company-tooltip-limit 15
     company-minimum-prefix-length 1
     ;; company-dabbrev-other-buffers t
     company-dabbrev-downcase t  ;;忽略大小写
     company-dabbrev-ignore-case t
     company-tooltip-align-annotations t
     company-begin-commands '(self-insert-command)
     company-global-modes '(not comint-mode erc-mode gud-mode rcirc-mode
                                minibuffer-inactive-mode))
    (add-hook 'after-init-hook 'global-company-mode))
  :config
  (progn
    (setq company-backends
          '((company-capf company-dabbrev-code  company-keywords company-files :with company-yasnippet)
            ;; (company-files)
            ;; (company-semantic)
            (company-dabbrev :with company-yasnippet)
            ))
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
  :bind (:map company-active-map
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("<RET>" . company-complete-selection)))


(use-package company-statistics
  :defer t
  :init
  (progn
    (unless (featurep 'company-statistics)
      (setq company-statistics-file (concat user-emacs-directory
                                            "company-statistics-cache.el"))
      (add-hook 'company-mode-hook 'company-statistics-mode)
      (company-statistics-mode 1))))

(use-package company-quickhelp
  :if (and t (display-graphic-p))
  :defer t
  :init
  (progn
    (add-hook 'company-mode-hook 'company-quickhelp-mode)
    (with-eval-after-load 'company
      (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends)))))

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

(provide 'init-company)
