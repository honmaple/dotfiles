(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
  (setq js2-basic-offset 4
        js-indent-level 4
        js2-bounce-indent-p nil
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

;;; Coffeescript
(use-package coffee-mode
  :mode ("\\.coffee\\.erb\\'" . coffee-mode)
  :config
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(use-package tern
  :diminish tern-mode
  :hook (js2-mode . tern-mode)
  :config (add-to-list 'tern-command "--no-port-file" 'append))

(use-package company-tern
  :init
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running `company-tern'."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language
               (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tern-mode (tern-mode))
            (if tern-mode (tern-mode -1))))))
  (maple/company-backend 'js2-mode-hook 'company-tern))

(provide 'init-js)
