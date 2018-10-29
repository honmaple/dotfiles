;;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

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
;; Basic configurations.
;;

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (maple-init . yas-global-mode)
  :config
  (setq yas-triggers-in-field t
        yas-prompt-functions '(yas-completing-prompt))
  (use-package yasnippet-snippets))

(use-package company
  :diminish company-mode " ⓐ"
  :hook (maple-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-tooltip-limit 15
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t ;; 提示右对齐
        company-dabbrev-downcase nil
        ;; company-transformers '(company-sort-by-occurrence) ;; 按使用频次排序
        company-begin-commands '(self-insert-command)
        company-global-modes '(not comint-mode
                                   erc-mode
                                   gud-mode
                                   rcirc-mode
                                   sql-interactive-mode
                                   minibuffer-inactive-mode
                                   inferior-python-mode
                                   shell-mode
                                   evil-command-window-mode))
  (defvar company-enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend)
                 (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (defvar company-default-backends
    (mapcar #'company-backend-with-yas
            '((company-dabbrev-code
               company-capf
               company-keywords
               company-files)
              (company-dabbrev
               company-gtags
               company-etags))))

  (setq company-backends company-default-backends)
  :custom-face
  (company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  :bind (:map company-active-map
              ("C-d" . company-show-doc-buffer)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle)
              ("<RET>" . company-complete-selection)))


(use-package company-statistics
  :hook (company-mode . company-statistics-mode)
  :config
  (setq company-statistics-file (concat maple-cache-directory
                                        "company-statistics.el")))

(use-package company-quickhelp
  :disabled
  :if (display-graphic-p)
  :commands company-quickhelp-manual-begin
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 1))

(provide 'init-company)

;;; init-company.el ends here
