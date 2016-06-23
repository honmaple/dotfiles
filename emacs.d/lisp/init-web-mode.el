(require-package 'web-mode)
(require-package 'smartparens)
(require-package 'company-web)
(require-package 'emmet-mode)



(use-package web-mode
  :defer t
  :config
  (progn
    (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-imenu-regexp-list
          '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
            ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
            ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
            (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "=")))
    (add-hook 'web-mode-hook
              (lambda ()
                (yas-global-mode 1)
                (add-to-list 'company-backends '(company-web-html :with company-yasnippet
                                                 company-web-slim
                                                 company-css
                                                 company-web-jade))
                (define-key evil-normal-state-local-map "za" 'web-mode-fold-or-unfold)
                (define-key evil-normal-state-local-map [f5] 'browse-url-of-file)
                )))
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.blade\\.php\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  )

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init
  (progn
    (add-hook 'js-mode-hook #'smartparens-mode)
    (add-hook 'web-mode-hook #'smartparens-mode)
    (add-hook 'css-mode-hook #'smartparens-mode)
    )
  :config
  (progn
    (with-eval-after-load 'smartparens
      (setq web-mode-enable-auto-pairing nil)
      (sp-local-pair 'web-mode "<% " " %>")
      ;; (sp-local-pair 'web-mode "{ " " }")
      (sp-local-pair 'web-mode "<%= "  " %>")
      (sp-local-pair 'web-mode "<%# "  " %>")
      (sp-local-pair 'web-mode "<%$ "  " %>")
      (sp-local-pair 'web-mode "<%@ "  " %>")
      (sp-local-pair 'web-mode "<%: "  " %>")
      (sp-local-pair 'web-mode "{% "  " %}")
      (sp-local-pair 'web-mode "{%- "  " %}")
      (sp-local-pair 'web-mode "{# "  " #}")
      )))


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
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
    (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
    ))

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (yas-global-mode 1)
;;             (jinja2-mode)
;;             ))
;; (add-hook 'jinja2-mode-hook 'web-mode-hook)
;; (add-hook 'web-mode-hook 'jinja2-mode-hook)

(provide 'init-web-mode)
