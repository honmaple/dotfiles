;;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

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
;; Lsp configurations.
;;

;;; Code:

(use-package lsp-mode
  :diminish lsp-mode
  :hook ((python-mode go-mode) . lsp)
  :config
  (setq lsp-prefer-flymake nil
        lsp-auto-guess-root t)

  ;; pip install python-language-server
  (use-package lsp-pyls
    :ensure nil
    :init
    (setq lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-pyflakes-enabled nil
          lsp-pyls-configuration-sources ["flake8"]
          lsp-clients-python-library-directories '("/usr/" "~/repo/python/lib/python3.7/")))

  ;; go get -u github.com/sourcegraph/go-langserver
  (use-package lsp-go
    :ensure nil)

  (use-package company-lsp
    :functions maple/company-backend
    :init (maple/company-backend 'lsp-mode-hook 'company-lsp)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t)
  (fset 'lsp-ui-flycheck-enable 'ignore)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(provide 'init-lsp)

;;; init-lsp.el ends here
