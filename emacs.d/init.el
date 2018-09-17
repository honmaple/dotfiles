;;; init.el --- Initialize configurations.	-*- lexical-binding: t -*-

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
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize); You may delete these explanatory comments.

;;; Code:

(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar user-default-theme nil)

(setq user-full-name "jianglin"
      user-default-theme 'monokai
      user-mail-address "mail@honmaple.com"
      gc-cons-threshold (* 256 1024 1024)
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-basic)
(require 'init-elpa)      ;; Machinery for installing required packages
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(defconst *common* t)
(defconst *develop* t)

(when *common*
  (require 'init-font)
  (require 'init-gui) ;;ui设置 显示行号
  (require 'init-ui)  ;; modeline,which-key

  (require 'init-editor) ;;自动补全括号等
  (require 'init-auto-insert)  ;;自动插入文件头
  (require 'init-evil)

  (require 'init-ivy)
  (require 'init-dired)   ;;自带文件管理
  (require 'init-file)   ;;文件操作
  (require 'init-buffer)   ;;buffer操作
  (require 'init-window))

(when *develop*
  (require 'init-flycheck)
  (require 'init-company)

  (require 'init-git)

  (require 'init-shell) ;;shell
  (require 'init-web)
  (require 'init-js)
  (require 'init-python)
  (require 'init-go)
  (require 'init-lua)
  (require 'init-c)
  (require 'init-sql)
  (require 'init-text) ;; markdown rst
  (require 'init-org)
  (require 'init-tool))

(with-eval-after-load 'evil-leader
  (require 'init-keybind))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------

(provide 'init)

;;; init.el ends here
