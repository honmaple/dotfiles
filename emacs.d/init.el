;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize); You may delete these explanatory comments.

(let ((file-name-handler-alist nil))
  (setq gc-cons-threshold (* 128 1024 1024))
  (setq user-full-name "jianglin")
  (setq user-mail-address  "xiyang0807@gmail.com")
  (setq inhibit-startup-echo-area-message "jianglin")

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  ;;----------------------------------------------------------------------------
  ;; Bootstrap config
  ;;----------------------------------------------------------------------------
  (require 'init-utils)
  (require 'init-elpa)      ;; Machinery for installing required packages
  ;;----------------------------------------------------------------------------
  ;; Load configs for specific features and modes
  ;;----------------------------------------------------------------------------
  (require-package 'diminish) ;;显示状态mode
  (require-package 'scratch) ;;缓冲区
  (require-package 'mwe-log-commands) ;; 命令行历史



  (defconst *system-is-mac* (eq system-type 'darwin))
  (defconst *system-is-linux* (eq system-type 'gnu/linux))
  (defconst *system-is-mswindows* (eq system-type 'windows-nt))
  (defconst *common* t)
  (defconst *develop* t)
   (defun maple/show-init-time ()
     (message "Emacs load finished in %.2fms"
              (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))))

   (add-hook 'after-init-hook 'maple/show-init-time)
  (when *common*
    (require 'init-fonts)
    (require 'init-ui)  ;; modeline,which-key
    (require 'init-gui) ;;ui设置 显示行号
    ;; (require 'init-console)

    (require 'init-evil)

    (when *system-is-mac*
      (require 'init-mac))


    (require 'init-editor) ;;自动补全括号等
    (require 'init-whitespace) ;;空白控制
    (require 'init-folding) ;;代码折叠
    (require 'init-auto-insert)  ;;自动插入文件头

    (require 'init-helm)
    (require 'init-dired)   ;;自带文件管理
    (require 'init-file)   ;;文件操作
    (require 'init-buffer)   ;;buffer操作
    (require 'init-windows)  ;;窗口管理C-x 2上下,C-x 3左右
    )

  (when *develop*
    (require 'init-flycheck)
    (require 'init-spelling)
    (require 'init-company)
    ;; (require 'init-gnus)

    (require 'init-git)
    (require 'init-rss)

    (require 'init-matlab)
    (require 'init-shell) ;;shell
    (require 'init-html)
    (require 'init-css)
    (require 'init-javascript)
    (require 'init-python)
    (require 'init-c)
    (require 'init-sql)
    (require 'init-text) ;; markdown rst
    (require 'init-org)
    (require 'init-tool)
    )

  (require 'init-keybind)

  ;;----------------------------------------------------------------------------
  ;; Variables configured via the interactive 'customize' interface
  ;;----------------------------------------------------------------------------
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))
  )

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))



(provide 'init)  

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
