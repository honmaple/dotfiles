;;; init-elpa.el --- Initialize elpa configurations.	-*- lexical-binding: t -*-

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
;; package configurations.
;; enter表示安装,d表示删除,x表示执行删除
;;

;;; Code:
;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                          ("org"   . "https://orgmode.org/elpa/")
;;                          ("gnu"   . "https://elpa.gnu.org/packages/")))

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org"   . "http://elpa.emacs-china.org/org/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/"))
      package-enable-at-startup nil
      package--init-file-ensured t)

;; (let ((default-directory "~/.emacs.d/elpa"))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t)))
;; (setq load-path (append load-path (list (expand-file-name "site-lisp" user-emacs-directory))))

(eval-when-compile
  (require 'maple-package)
  (maple-package/initialize 'no-activate)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Setup `use-package'
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose nil
        use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t
        use-package-minimum-reported-time 0.01))

(use-package evil-use-package
  :ensure nil
  :demand)

;;显示状态mode
(use-package diminish)

;;缓冲区
(use-package scratch)

(use-package async-bytecomp
  :ensure async
  :hook (maple-init . async-bytecomp-package-mode)
  :config
  (setq async-bytecomp-allowed-packages '(all)))

;; (use-package benchmark-init
;;   :init (benchmark-init/activate)
;;   :hook (after-init . benchmark-init/deactivate))

(use-package package-utils
  :commands (package-utils-upgrade-all)
  :init
  (defalias 'package-upgrade 'package-utils-upgrade-all))

(use-package cl-lib)
(use-package fullframe)
(use-package restart-emacs
  :init
  (defun maple/restart-emacs()
    "Restart Emacs."
    (interactive)
    (setq restart-emacs-restore-frames t)
    (restart-emacs)))

(use-package exec-path-from-shell
  :if maple-system-is-mac
  :init (exec-path-from-shell-initialize))

(use-package server
  :ensure nil
  :commands (server-running-p)
  :hook (maple-init . (lambda() (unless (server-running-p) (server-start)))))

(provide 'init-elpa)
;;; init-elpa.el ends here
