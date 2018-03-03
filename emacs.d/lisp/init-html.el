(use-package company-web)

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2
          web-mode-enable-auto-closing t ; enable auto close tag in text-mode
          web-mode-enable-current-element-highlight t
          web-mode-enable-auto-indentation nil
          web-mode-enable-control-block-indentation nil
          web-mode-enable-css-colorization nil
          web-mode-engines-alist '(("django" . "\\.html\\'")
                                   ("django" . "\\.vue\\'"))
          web-mode-engines-auto-pairs '(("django" . (("{{ " . " }")
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
    (maple/company-backend 'web-mode-hook '(company-web-html
                                            company-css
                                            company-tern))
    )
  :evil-bind
  (normal web-mode-map
          (kbd "<f5>") 'browse-url-of-file
          ;; (kbd "<f6>") 'web-beautify-html
          (kbd "za") 'web-mode-fold-or-unfold))

(use-package web-beautify
  :commands (web-beautify-html web-beautify-css web-beautify-js))

(use-package emmet-mode
  :diminish emmet-mode
  :hook ((html-mode sgml-mode web-mode) . emmet-mode)
  :config
  (defun maple/emmet-expand ()
    (interactive)
    (if (bound-and-true-p yas-minor-mode)
        (call-interactively 'emmet-expand-yas)
      (call-interactively 'emmet-expand-line)))
  :evil-bind
  (insert emmet-mode-keymap
          (kbd "TAB") 'maple/emmet-expand
          (kbd "<tab>") 'maple/emmet-expand))

(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (maple/company-backend 'css-mode-hook 'company-css))

(use-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :config (setq-default scss-compile-at-save nil)
  :mode ("\\.scss\\'" . scss-mode))


(use-package less-css-mode
  :mode ("\\.less\\'" . less-css-mode))


(provide 'init-html)
