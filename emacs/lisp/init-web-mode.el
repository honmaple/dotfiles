(require-package 'web-mode)
(require-package 'smartparens)
(require-package 'company-web)


(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;; 自动添加{% %},jinja2不错
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'css-mode-hook #'smartparens-mode)
;; make web-mode play nice with smartparens
(with-eval-after-load 'smartparens
                      (setq web-mode-enable-auto-pairing nil)
                      (sp-local-pair 'web-mode "<% " " %>")
                      (sp-local-pair 'web-mode "{ " " }")
                      (sp-local-pair 'web-mode "<%= "  " %>")
                      (sp-local-pair 'web-mode "<%# "  " %>")
                      (sp-local-pair 'web-mode "<%$ "  " %>")
                      (sp-local-pair 'web-mode "<%@ "  " %>")
                      (sp-local-pair 'web-mode "<%: "  " %>")
                      (sp-local-pair 'web-mode "{{ "  " }}")
                      (sp-local-pair 'web-mode "{% "  " %}")
                      (sp-local-pair 'web-mode "{%- "  " %}")
                      (sp-local-pair 'web-mode "{# "  " #}"))

(eval-after-load 'web-mode
                 '(progn
                    (push '(company-web-html 
                             company-web-slim
                             company-css 
                             company-web-jade) company-backends)
                    ; (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
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
                                (define-key evil-normal-state-local-map "za" 'web-mode-fold-or-unfold)))
                    ))

(require-package 'emmet-mode)
(require 'emmet-mode) ;;必须的
(eval-after-load 'emmet-mode
                 '(progn
                    (add-hook 'html-mode-hook 'emmet-mode)
                    (add-hook 'sgml-mode-hook 'emmet-mode)
                    (add-hook 'web-mode-hook 'emmet-mode)
                    (add-hook 'css-mode-hook  'emmet-mode)
                    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
                    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
                    (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
                    (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
                    (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
                    (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
                    ))
(provide 'init-web-mode)
