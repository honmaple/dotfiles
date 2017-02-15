(require-package 'elpy)
(require-package 'pip-requirements)

(use-package pip-requirements
  :defer t
  :diminish pip-requirements-mode)

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :init
  (progn
    ;; (setq python-indent-offset 4)
    (setq imenu-create-index-function 'semantic-create-imenu-index)
    (setq electric-indent-chars (delq ?: electric-indent-chars)))
  :config
  (progn
    (set-variable 'python-indent-offset 4)
    (set-variable 'python-indent-guess-indent-offset nil)
    (setq tab-width 4)
    (setq python-shell-completion-native-enable nil)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i")
    ))

(use-package yapfify
  :ensure t
  :defer t
  ;; 保存时自动格式化
  ;; :init (add-hook 'python-mode-hook 'yapf-mode)
  :init
  (progn
    (evil-define-key 'normal python-mode-map
      (kbd "<f6>") 'yapfify-buffer)
    ))

(use-package pyvenv
  :defer t)

(use-package anaconda-mode
  :defer t
  :init
  (progn
    (setq anaconda-mode-installation-directory
          (concat maple-cache-directory "anaconda-mode"))
    (add-hook 'python-mode-hook 'anaconda-mode)
    )
  :diminish anaconda-mode
  :config
  (progn
    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      (evil--jumps-push))
    (evil-define-key 'normal anaconda-mode-map
      (kbd "gd") 'anaconda-mode-find-assignments)
    (evil-define-key 'normal anaconda-mode-view-mode-map
      (kbd "q") 'quit-window)
    ))


(use-package company-anaconda
  :after anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook
              (lambda () (maple/add-to-company-backend '(company-anaconda))))
    ))


;; (use-package elpy
;;   :after python
;;   :diminish elpy-mode "ⓔ"
;;   :config
;;   (progn
;;     (push '("*Python*") popwin:special-display-config)
;;     ;; (setq python-shell-interpreter "python")
;;     (setenv "PYTHONPATH" "$PYTHONPATH:/usr/lib/python3.5/site-packages")
;;     (setq python-shell-completion-native-enable nil)
;;     (setq python-shell-interpreter "ipython"
;;           python-shell-interpreter-args "--simple-prompt -i")
;;     (remove-hook 'elpy-modules 'elpy-module-flymake)
;;     ;; (setq elpy-rpc-backend "jedi")
;;     (remove-hook 'elpy-modules 'elpy-module-company)
;;     (setq shell-file-name "/bin/bash")
;;     (elpy-enable)
;;     (add-hook 'python-mode-hook
;;               (lambda ()
;;                 (define-key evil-normal-state-local-map [f6] 'elpy-yapf-fix-code)
;;                 (define-key evil-normal-state-local-map [f5] 'elpy-shell-send-region-or-buffer)
;;                 (define-key evil-normal-state-local-map "gd" 'elpy-goto-definition)))
;;     ))

;; (use-package jinja2-mode
;;   :commands (jinja2-mode)
;;   :mode ("\\.html\\'" . jinja2-mode)
;;   :init
;;   (progn
;;     (after-load 'smartparens
;;       (sp-local-pair 'jinja2-mode "{% "  " %}"))
;;     ))
(provide 'init-python)
