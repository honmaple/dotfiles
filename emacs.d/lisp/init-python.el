(require-package 'elpy)
;; (require-package 'company-jedi)
;; (require-package 'py-yapf)
(require-package 'pip-requirements)

(use-package elpy
  :after python
  :diminish elpy-mode "ⓔ"
  :mode ("\\.py\\'" . python-mode)
  :config
  (progn
    (elpy-enable)
    (set-variable 'python-indent-offset 4)
    (set-variable 'python-indent-guess-indent-offset nil)
    (add-hook 'python-mode-hook
              (lambda ()
                (setq tab-width 4)
                ;; (setq python-indent-offset 4)
                (setq electric-indent-chars (delq ?: electric-indent-chars))
                (define-key evil-normal-state-local-map [f6] 'elpy-yapf-fix-code)
                (define-key evil-normal-state-local-map [f5] 'elpy-shell-send-region-or-buffer)
                (define-key evil-normal-state-local-map "gd" 'elpy-goto-definition)))
    ;; (make-local-variable 'company-backends)
    ;; (setq company-backends (copy-tree company-backends))
    ;; (setf (car company-backends)
    ;;       (append '(company-jedi :with company-yasnippet) (car company-backends)))
    (push '("*Python*") popwin:special-display-config)

    (with-eval-after-load 'elpy
      ;; (setq python-shell-interpreter "ipython")
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      ;; (remove-hook 'elpy-modules 'elpy-module-company)
      )
    ))

;; (use-package py-yapf
;;   :init
;;   :config
;;   (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

;; (use-package jinja2-mode
;;   :commands (jinja2-mode)
;;   :mode ("\\.html\\'" . jinja2-mode)
;;   :init
;;   (progn
;;     (with-eval-after-load 'smartparens
;;       (sp-local-pair 'jinja2-mode "{% "  " %}"))
;;     ))
(provide 'init-python)
