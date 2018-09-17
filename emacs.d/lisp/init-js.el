;;; init-js.el --- Initialize js configurations.	-*- lexical-binding: t -*-

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
;; Javascript configurations.
;;

;;; Code:

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
  (setq js2-basic-offset 4
        js-indent-level 4
        js2-bounce-indent-p nil
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  (use-package tern
    :diminish tern-mode
    :hook (js2-mode . tern-mode)
    :config (add-to-list 'tern-command "--no-port-file" 'append))

  (use-package company-tern
    :functions maple/company-backend
    :init (maple/company-backend 'js2-mode-hook 'company-tern)))

;;; Coffeescript
(use-package coffee-mode
  :mode ("\\.coffee\\.erb\\'" . coffee-mode)
  :config
  (setq coffee-tab-width 4))

(provide 'init-js)

;;; init-js.el ends here
