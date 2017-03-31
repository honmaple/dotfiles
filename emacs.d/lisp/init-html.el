(require-package 'web-mode)
(require-package 'smartparens)
(require-package 'company-web)
(require-package 'emmet-mode)

(use-package web-mode
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
    (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
    (setq web-mode-enable-current-element-highlight t)
    (maple/add-to-company-backend '(company-web-html company-css) 'web-mode-hook)
    (evil-define-key 'normal web-mode-map
      (kbd "<f5>") 'browse-url-of-file
      ;; (kbd "<f6>") 'web-beautify-html
      (kbd "za") 'web-mode-fold-or-unfold)))

(use-package smartparens
  :defer t
  :after web-mode
  :diminish smartparens-mode
  :init
  (progn
    (add-hook 'js-mode-hook #'smartparens-mode)
    (add-hook 'web-mode-hook #'smartparens-mode)
    (add-hook 'css-mode-hook #'smartparens-mode)
    )
  :config
  (progn
    (sp-local-pair 'web-mode "<% " " %>")
    (sp-local-pair 'web-mode "<%= "  " %>")
    (sp-local-pair 'web-mode "<%# "  " %>")
    (sp-local-pair 'web-mode "<%$ "  " %>")
    (sp-local-pair 'web-mode "<%@ "  " %>")
    (sp-local-pair 'web-mode "<%: "  " %>")
    (sp-local-pair 'web-mode "{% "  " %}")
    (sp-local-pair 'web-mode "{%- "  " %}")
    (sp-local-pair 'web-mode "{# "  " #}")
    ))


(use-package emmet-mode
  :defer t
  :diminish emmet-mode
  :init
  (progn
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode))
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


;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (yas-global-mode 1)
;;             (jinja2-mode)
;;             ))
;; (add-hook 'jinja2-mode-hook 'web-mode-hook)
;; (add-hook 'web-mode-hook 'jinja2-mode-hook)


(provide 'init-html)