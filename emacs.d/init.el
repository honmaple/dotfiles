;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-compat) ;;兼容性问题
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
; (require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

; (require-package 'wgrep)
; (require-package 'project-local-variables)
(require-package 'diminish) ;;显示状态mode
(require-package 'scratch) ;;缓冲区
(require-package 'mwe-log-commands) ;; 命令行历史

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-linum-mode) ;;显示行号
(require 'init-gui-frames)
(require 'init-fonts)


(require 'init-recentf)
(require 'init-helm)
(require 'init-dired)   ;;自带文件管理
(require 'init-hippie-expand)  ;;emacs自带补全 M-/
(require 'init-windows)  ;;窗口管理C-x 2上下,C-x 3左右
(require 'init-modeline)   ;;状态栏

(require 'init-editing-utils) ;;自动补全括号等
(require 'init-whitespace) ;;空白控制
(require 'init-hs-minor-mode) ;;代码折叠
(require 'init-fci) ;;这个应该是代码缩进线,代码格式化
(require 'init-auto-insert) 
; (require 'init-shell) 

(require 'init-git)

(require 'init-evil)
(require 'init-keybind)

(require 'init-markdown)
(require 'init-rst)
(require 'init-matlab)
(require 'init-sh) ;;shell
(require 'init-web-mode)
(require 'init-javascript)
(require 'init-css)
(require 'init-python)

(require 'init-org)

(require 'init-lisp)
; (require 'init-slime)
; (require 'init-common-lisp) ;; lisp

(require 'init-flycheck)
;; (require 'init-spelling)
(require 'init-yasnippet)
(require 'init-company)


(require 'init-misc)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
;;自定义主题
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales) ;;一些编码设置

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
