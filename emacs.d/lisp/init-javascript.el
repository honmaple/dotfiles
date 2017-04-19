(use-package js2-mode
  :ensure t
  :defer t
  :mode
  ("\\.js\\'" . js2-mode)
  :config
  (progn
    (setq-default js2-basic-offset 2
                  js2-bounce-indent-p nil)
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    (setq-default js-indent-level 2)
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
    ))

(use-package tern
  :defer t
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config
  (progn
    (add-to-list 'tern-command "--no-port-file" 'append)
    ))

(use-package json-mode
  :ensure t
  :defer t)

;;; Coffeescript
(use-package coffee-mode
  :ensure t
  :defer t
  :mode ("\\.coffee\\.erb\\'" . coffee-mode)
  :config
  (progn
    (setq coffee-js-mode preferred-javascript-mode
          coffee-tab-width preferred-javascript-indent-level)
    ))

(use-package tern
  :ensure t
  :defer t
  :diminish tern-mode
  :init (add-hook 'js2-mode-hook 'tern-mode)
  :config (add-to-list 'tern-command "--no-port-file" 'append))

(use-package company-tern
  :ensure t
  :defer t
  :init
  (progn
    (defadvice company-tern (before web-mode-set-up-ac-sources activate)
      "Set `tern-mode' based on current language before running `company-tern'."
      (if (equal major-mode 'web-mode)
          (let ((web-mode-cur-language
                 (web-mode-language-at-pos)))
            (if (or (string= web-mode-cur-language "javascript")
                    (string= web-mode-cur-language "jsx"))
                (unless tern-mode (tern-mode))
              (if tern-mode (tern-mode -1))))))
    ;; (maple/add-to-company-backend '(company-tern) 'web-mode-hook)
    (maple/add-to-company-backend '(company-tern) 'js2-mode-hook)
    ))

(provide 'init-javascript)
