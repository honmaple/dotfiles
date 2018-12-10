;;; init-text.el --- Initialize text configurations.	-*- lexical-binding: t -*-

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
;; TEXT configurations.
;;

;;; Code:
(use-package markdown-mode
  :config
  (use-package org-table
    :ensure nil
    :diminish orgtbl-mode
    :hook (markdown-mode . orgtbl-mode)
    :config
    (when (display-graphic-p) (set-face-attribute 'org-table nil :font "Inconsolata 12")))

  (defun cleanup-org-tables ()
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "-+-" nil t) (replace-match "-|-"))))

  (maple/add-hook 'markdown-mode-hook
    (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local))

  (use-package markdown-toc)

  (use-package markdown-preview-mode
    :ensure nil
    :commands (markdown-preview-mode))

  :bind
  (:map markdown-mode-map
        ([f5] . markdown-toggle-markup-hiding)))

(use-package company-english-helper
  :ensure nil
  :functions maple/company-backend
  :commands (company-english-helper-search)
  :init
  (maple/company-backend '(org-mode-hook markdown-mode-hook) 'company-english-helper-search t)
  :setq
  (:mode org-mode
         company-tooltip-align-annotations nil)
  (:mode markdown-mode
         company-tooltip-align-annotations nil))

(use-package yaml-mode)
(use-package vimrc-mode)
(use-package json-mode)
(use-package writeroom-mode
  :config
  (setq writeroom-mode-line t
        writeroom-bottom-divider-width 0))

(provide 'init-text)
;;; init-text.el ends here
