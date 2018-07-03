;;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 lin.jiang

;; Author: lin.jiang <xiyang0807@gmail.com>
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

(use-package elpy
  :disabled)

(use-package pip-requirements
  :diminish pip-requirements-mode)

(use-package python
  :ensure nil
  :setq
  (:mode python-mode
         imenu-create-index-function 'semantic-create-imenu-index
         electric-indent-chars (delq ?: electric-indent-chars))
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-shell-completion-native-enable nil)
  ;; python-shell-interpreter "ipython"
  ;; python-shell-interpreter-args "--simple-prompt -i")
  (setenv "PYTHONPATH" "$PYTHONPATH:/usr/lib/python3.6/site-packages:$HOME/.local/lib/python3.6/site-packages")
  (defun maple/run-python ()
    (interactive)
    (python-shell-get-or-create-process)
    (if (region-active-p)
        (python-shell-send-region (region-beginning) (region-end) t)
      (python-shell-send-buffer t)))
  (add-hook 'inferior-python-mode-hook 'maple/close-process)
  :evil-state (inferior-python-mode . insert)
  :bind (:map python-mode-map
              ([f5] . maple/run-python)))

(use-package py-isort)
(use-package pyvenv)

(use-package yapfify
  ;; 保存时自动格式化
  ;; :hook (python-mode . yapf-mode)
  :evil-bind
  (normal python-mode-map
          [f6] 'yapfify-buffer))


(use-package anaconda-mode
  :diminish anaconda-mode
  :evil-state (anaconda-mode-view-mode . emacs)
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  :config
  (setq anaconda-mode-installation-directory
        (concat maple-cache-directory "anaconda-mode"))
  :evil-bind
  (normal anaconda-mode-map
          (kbd "gd") 'anaconda-mode-find-assignments))


(use-package company-anaconda
  :init (maple/company-backend 'anaconda-mode-hook 'company-anaconda))

(provide 'init-python)

;;; init-python.el ends here
