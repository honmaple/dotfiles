;;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

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
;; Python configurations.
;;

;;; Code:
(use-package python
  :ensure nil
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-shell-completion-native-enable nil)
  (defun maple/run-python ()
    (interactive)
    (or (python-shell-get-process) (call-interactively 'run-python))
    (if (use-region-p)
        (python-shell-send-region (region-beginning) (region-end) t)
      (python-shell-send-buffer t)))

  (use-package pip-requirements
    :diminish pip-requirements-mode)

  (use-package py-isort)

  (use-package pyvenv)

  (use-package yapfify
    :commands (yapfify-buffer))

  :custom
  (:language
   "python-mode"
   :run        'maple/run-python
   :indent 'yapfify-buffer))

(use-package anaconda-mode
  :diminish anaconda-mode
  :unless *lsp*
  :hook ((python-mode . anaconda-mode)
         (anaconda-mode . anaconda-eldoc-mode))
  :config
  (setq anaconda-mode-installation-directory
        (concat maple-cache-directory "anaconda-mode"))

  (use-package company-anaconda
    :functions maple/company-backend
    :init (maple/company-backend 'anaconda-mode-hook 'company-anaconda))
  :custom
  (:language
   "anaconda-mode"
   :definition 'anaconda-mode-find-assignments))

(provide 'init-python)

;;; init-python.el ends here
