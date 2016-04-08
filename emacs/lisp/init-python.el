; (require-package 'anaconda-mode)
(require-package 'elpy)
(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

(defun python-mode-hook-setup ()
  ;; run command `pip install jedi flake8 importmagic` in shell,
  ;; or just check https://github.com/jorgenschaefer/elpy
  (elpy-enable)
  ; (setq elpy-rpc-python-command "python3")
  ; (elpy-use-cpython "python3")
  ; (setq python-shell-interpreter "python3")
  ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
  ;; emacs 24.4 only
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  )

(add-hook 'python-mode-hook 'python-mode-hook-setup)
(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (set-variable 'python-indent-offset 4)
            (set-variable 'python-indent-guess-indent-offset nil)
            (define-key evil-normal-state-local-map [f6] 'elpy-yapf-fix-code)
            (define-key evil-normal-state-local-map [f5] 'elpy-shell-send-region-or-buffer)))


(provide 'init-python)
