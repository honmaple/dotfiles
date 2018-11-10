;;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Web configurations.
;;

;;; Code:

(use-package web-mode
  :mode ("\\.\\(vue\\|html?\\)$")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-enable-auto-closing t ; enable auto close tag in text-mode
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-indentation nil
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
                                                   ("<% " . " %>")))))

  (fset 'maple/put-text-property (symbol-function 'put-text-property))
  (defun maple/web-mode-put-text(p q prop value)
    (if (and (eq prop 'invisible) value) (hs-make-overlay p q 'code)
      (maple/put-text-property p q prop value)))
  (defun maple/web-mode-fold-or-unfold()
    (interactive)
    (cl-letf (((symbol-function 'put-text-property) 'maple/web-mode-put-text))
      (web-mode-fold-or-unfold)))
  (maple/add-hook 'web-mode-hook
    (setq electric-pair-pairs '((?\' . ?\'))))
  :evil-bind
  (normal web-mode-map
          ([f5] . browse-url-of-file)
          ("za" . maple/web-mode-fold-or-unfold)))

(use-package company-web
  :functions maple/company-backend
  :init
  (maple/company-backend 'css-mode-hook 'company-css)
  (maple/company-backend 'web-mode-hook '(company-web-html
                                          company-css
                                          company-tern)))

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
          ([tab] . maple/emmet-expand)))

(use-package css-mode
  :config
  (setq css-indent-offset 4))

(use-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :config (setq scss-compile-at-save nil))

(use-package less-css-mode
  :mode ("\\.less\\'" . less-css-mode))

(provide 'init-web)

;;; init-web.el ends here
