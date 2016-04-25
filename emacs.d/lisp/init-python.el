(require-package 'elpy)
;; (autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)
;; (setq interpreter-mode-alist
;;       (cons '("python" . python-mode) interpreter-mode-alist))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (after-load 'python-mode
;;   )

(with-eval-after-load 'python
                                        ; (setq-default indent-tabs-mode t)
                                        ; (setq-default tab-width 4)
                                        ; (setq-default py-indent-tabs-mode t)
  (setq tab-width 4)
  (set-variable 'python-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)
  (require 'elpy)
  (elpy-enable)
  (setq electric-indent-chars (delq ?: electric-indent-chars)))

(with-eval-after-load 'elpy
  (define-key evil-normal-state-map [f6] 'elpy-yapf-fix-code)
  (define-key evil-normal-state-map [f5] 'elpy-shell-send-region-or-buffer)
  (remove-hook 'elpy-modules 'elpy-module-flymake))


;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq tab-width 4)
;;             (set-variable 'python-indent-offset 4)
;;             (set-variable 'python-indent-guess-indent-offset nil)
;;             (require 'elpy)
;;             (elpy-enable)
;;             (setq electric-indent-chars (delq ?: electric-indent-chars))
;;             (define-key evil-normal-state-local-map [f6] 'elpy-yapf-fix-code)
;;             (define-key evil-normal-state-local-map [f5] 'elpy-shell-send-region-or-buffer)))


(provide 'init-python)
