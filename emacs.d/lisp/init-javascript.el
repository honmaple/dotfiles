(require-package 'json-mode)
(require-package 'js2-mode)
(require-package 'coffee-mode)
(require-package 'company-tern)
(require-package 'tern)


(use-package js2-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
  :config
  (progn
    (setq-default js2-basic-offset 2
                  js2-bounce-indent-p nil)
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    (setq-default js-indent-level 2)
    ))

;; Javascript nests {} and () a lot, so I find this helpful

;; (use-package rainbow-delimiters
;;   :defer t
;;   :init
;;   (progn
;;     (dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
;;       (add-hook hook 'rainbow-delimiters-mode))
;;     ))

(use-package json-mode
  :defer t)

;;; Coffeescript
(use-package coffee-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode))
  :config
  (progn
    (setq coffee-js-mode preferred-javascript-mode
          coffee-tab-width preferred-javascript-indent-level)
    ))

(use-package tern
  :defer t
  :diminish tern-mode
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config (add-to-list 'tern-command "--no-port-file" 'append))

(use-package company-tern
  :defer t
  :init
  (progn
    (defadvice company-tern (before web-mode-set-up-ac-sources activate)
      "Set `tern-mode' based on current language before running `company-tern'."
      (if (equal major-mode 'web-mode)
          (let ((web-mode-cur-language (web-mode-language-at-pos)))
            (if (or (string= web-mode-cur-language "javascript")
                    (string= web-mode-cur-language "jsx"))
                (unless tern-mode (tern-mode))
              (if tern-mode (tern-mode -1))
              ))))
    (add-hook 'web-mode-hook
              (lambda()
                (make-local-variable 'company-backends)
                (setq company-backends (copy-tree company-backends))
                (setf (car company-backends)
                      (append '(company-tern) (car company-backends)))
                ))
    (add-hook 'js2-mode-hook
              (lambda()
                ;; (add-to-list (make-local-variable 'company-backends)
                ;;              'company-tern)
                (make-local-variable 'company-backends)
                (setq company-backends (copy-tree company-backends))
                (setf (car company-backends)
                      (append '(company-tern) (car company-backends)))
                ))
    ))

(provide 'init-javascript)
