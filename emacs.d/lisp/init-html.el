(use-package company-web
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.blade\\.php\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
    (setq web-mode-enable-current-element-highlight nil)
    (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-enable-css-colorization nil)
    ;; see https://github.com/fxbois/web-mode/issues/275
    ;; (setq web-mode-enable-auto-pairing nil)
    ;; (add-hook
    ;;  'web-mode-hook
    ;;  '(lambda ()
    ;;     (setq-local electric-pair-inhibit-predicate
    ;;                 (lambda (c)
    ;;                   (if (char-equal c ?{) t (electric-pair-default-inhibit c))))))
    (setq web-mode-engines-alist '(("django" . "\\.html\\'")
                                   ("django" . "\\.vue\\'")))
    (setq web-mode-engines-auto-pairs '(("django" . (("{{ " . " }")
                                                     ("{% " . " %")
                                                     ("{%-" . " | %")
                                                     ("{%=" . " | %")
                                                     ("{{-" . " | }")
                                                     ("{{{" . " | }}")
                                                     ("{# " . " #")
                                                     ("<% " . " %>")
                                                     ))))
    (add-hook 'web-mode-hook
              (lambda ()
                (setq electric-pair-pairs '((?\' . ?\')))))
    (maple/add-to-company-backend '(company-web-html
                                    company-css
                                    company-tern) 'web-mode-hook)
    (evil-define-key 'normal web-mode-map
      (kbd "<f5>") 'browse-url-of-file
      ;; (kbd "<f6>") 'web-beautify-html
      (kbd "za") 'web-mode-fold-or-unfold)))

(use-package web-beautify
  :ensure t
  :commands (web-beautify-html web-beautify-css web-beautify-js)
  )

(use-package emmet-mode
  :ensure t
  :defer t
  :diminish emmet-mode
  :init
  (progn
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    ;; (add-hook 'css-mode-hook  'emmet-mode)
    )
  :config
  (progn
    (defun maple/emmet-expand ()
      (interactive)
      (if (bound-and-true-p yas-minor-mode)
          (call-interactively 'emmet-expand-yas)
        (call-interactively 'emmet-expand-line)))
    (evil-define-key 'insert emmet-mode-keymap
      (kbd "TAB") 'maple/emmet-expand
      (kbd "<tab>") 'maple/emmet-expand)
    ))

(use-package css-mode
  :ensure t
  :defer t
  :config
  (progn
    (maple/add-to-company-backend '(company-css) 'css-mode-hook)
    (put 'css-indent-offset 'safe-local-variable #'integerp)
    ))

(use-package sass-mode
  :ensure t
  :defer t
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :ensure t
  :defer t
  :config (setq-default scss-compile-at-save nil)
  :mode ("\\.scss\\'" . scss-mode))


(use-package less-css-mode
  :ensure t
  :defer t
  :mode ("\\.less\\'" . less-css-mode))


(provide 'init-html)
