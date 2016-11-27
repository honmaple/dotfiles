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
    (set-variable 'python-indent-offset 4)
    (set-variable 'python-indent-guess-indent-offset nil)
    (setq tab-width 4)
    ;; (setq python-indent-offset 4)
    (setq imenu-create-index-function 'semantic-create-imenu-index)
    (setq electric-indent-chars (delq ?: electric-indent-chars))))



(use-package elpy
  :after python
  :diminish elpy-mode "ⓔ"
  :config
  (progn
    (elpy-enable)
    (push '("*Python*") popwin:special-display-config)
    ;; (setq python-shell-interpreter "python")
    (setq python-shell-completion-native-enable nil)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i")
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    ;; (remove-hook 'elpy-modules 'elpy-module-company)
    (setq shell-file-name "/bin/bash")
    (add-hook 'python-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map [f6] 'elpy-yapf-fix-code)
                (define-key evil-normal-state-local-map [f5] 'elpy-shell-send-region-or-buffer)
                (define-key evil-normal-state-local-map "gd" 'elpy-goto-definition)))
    ))

;; (use-package jinja2-mode
;;   :commands (jinja2-mode)
;;   :mode ("\\.html\\'" . jinja2-mode)
;;   :init
;;   (progn
;;     (with-eval-after-load 'smartparens
;;       (sp-local-pair 'jinja2-mode "{% "  " %}"))
;;     ))
(provide 'init-python)
