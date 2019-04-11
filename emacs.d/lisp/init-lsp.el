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
  :config
  (setq lsp-inhibit-message t
        lsp-message-project-root-warning t
        lsp-prefer-flymake nil)

  (use-package lsp-go
    :hook (go-mode . lsp-go-enable))

  (use-package lsp-python
    :hook (python-mode . lsp-python-enable))

  (use-package company-lsp
    :functions maple/company-backend
    :init (maple/company-backend 'lsp-mode-hook 'company-lsp)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)))

(provide 'init-lsp)

;;; init-lsp.el ends here
