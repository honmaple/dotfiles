;;; init.el --- user init configuration.

;;; Commentary:
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize); You may delete these explanatory comments.


;;; Code:
(defvar default-file-name-handler-alist file-name-handler-alist)

(setq user-full-name "jianglin")
(setq user-default-theme 'doom-one)
(setq user-mail-address "xiyang0807@gmail.com")
(setq file-name-handler-alist nil)
(setq gc-cons-threshold (* 256 1024 1024))
(setq inhibit-startup-echo-area-message "jianglin")

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(defadvice require (around require activate)
  "Show require time."
  (let ((start (current-time))
        res delta)
    (setq res ad-do-it)
    (setq delta (float-time (time-since start)))
    (when (> delta 0.1)
      (message "Required %s: %s sec" (ad-get-arg 0) delta))
    res))

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
  (require 'init-fonts)
  (require 'init-gui) ;;ui设置 显示行号
  (require 'init-ui)  ;; modeline,which-key

  (require 'init-editor) ;;自动补全括号等
  (require 'init-auto-insert)  ;;自动插入文件头
  (require 'init-evil)

  (require 'init-helm)
  ;; (require 'init-ivy)
  (require 'init-dired)   ;;自带文件管理
  (require 'init-file)   ;;文件操作
  (require 'init-buffer)   ;;buffer操作
  (require 'init-windows))


(when *develop*
  (require 'init-flycheck)
  (require 'init-spelling)
  (require 'init-company)

  (require 'init-git)

  (require 'init-shell) ;;shell
  (require 'init-html)
  (require 'init-js)
  (require 'init-python)
  (require 'init-go)
  (require 'init-c)
  (require 'init-sql)
  (require 'init-text) ;; markdown rst
  (require 'init-org)
  (require 'init-tool))

(after-load 'evil-leader
  (require 'init-keybind))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------

(provide 'init)

;;; init.el ends here
