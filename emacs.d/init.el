;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq gc-cons-threshold (* 128 1024 1024))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'diminish) ;;显示状态mode
(require-package 'scratch) ;;缓冲区
(require-package 'mwe-log-commands) ;; 命令行历史
(require-package 'use-package)


(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *common* t)
(defconst *develop* t)


(when *common*
  (require 'init-frame-hooks)
  (require 'init-gui) ;;ui设置 显示行号
  (require 'init-ui)  ;; modeline,which-key
  (require 'init-fonts)
  (require 'init-locales) ;;一些编码设置


  (when *is-a-mac*
    (require 'init-mac))


  (require 'init-evil)
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
  ;; (require 'init-spelling)
  (require 'init-yasnippet)
  (require 'init-company)
  (require 'init-quickrun)
  ;; ;; (require 'init-gnus)

  (require 'init-git)
  (require 'init-rss)

  (require 'init-matlab)
  (require 'init-shell) ;;shell
  (require 'init-web-mode)
  (require 'init-javascript)
  (require 'init-css)
  (require 'init-python)
  (require 'init-c)
  (require 'init-sql)
  (require 'init-text) ;; markdown rst
  (require 'init-org)
  (require 'init-tool)
  )


(require 'init-keybind)

;; (require 'init-misc) ;;yes or no

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)  

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
