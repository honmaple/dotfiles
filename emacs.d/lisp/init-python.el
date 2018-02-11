(use-package elpy
  :ensure t
  :disabled)

(use-package pip-requirements
  :ensure t
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
    (setenv "PYTHONPATH" "$PYTHONPATH:/usr/lib/python3.5/site-packages")
    (setq python-shell-completion-native-enable nil)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i")
    (defun maple/run-python ()
      (interactive)
      (python-shell-get-or-create-process)
      (if (region-active-p)
          (python-shell-send-region (region-beginning) (region-end) t)
        (python-shell-send-buffer t)))
    (add-hook 'inferior-python-mode-hook 'maple/close-process))
  :bind (:map python-mode-map
              ([f5] . maple/run-python)))

(use-package py-isort
  :ensure t
  :defer t)


(use-package yapfify
  :ensure t
  :defer t
  ;; 保存时自动格式化
  ;; :init (add-hook 'python-mode-hook 'yapf-mode)
  :evil-bind
  (normal python-mode-map
          [f6] 'yapfify-buffer))

(use-package pyvenv
  :ensure t
  :defer t)

(use-package anaconda-mode
  :ensure t
  :defer t
  :diminish anaconda-mode
  :evil-state
  (inferior-python-mode . insert)
  (anaconda-mode-view-mode . emacs)
  :init
  (progn
    (setq anaconda-mode-installation-directory
          (concat maple-cache-directory "anaconda-mode"))
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    )
  :config
  (progn
    (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      (evil--jumps-push))
    ;; (maple/set-quit-key anaconda-mode-view-mode-map)
    )
  :evil-bind
  (normal anaconda-mode-map
          (kbd "gd") 'anaconda-mode-find-assignments))


(use-package company-anaconda
  :ensure t
  :after anaconda-mode
  :init (maple/add-to-company-backend 'company-anaconda 'python-mode-hook))

(provide 'init-python)
