(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
;; (require 'init-exec-path) ;; Set up $PATH
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'diminish) ;;显示状态mode
(require-package 'scratch) ;;缓冲区
(require-package 'mwe-log-commands) ;; 命令行历史
(require-package 'use-package)

(require 'init-frame-hooks)
(require 'init-themes)
(require 'init-gui) ;;ui设置 显示行号
(require 'init-fonts)

(require 'init-editing-utils) ;;自动补全括号等
(require 'init-whitespace) ;;空白控制
(require 'init-hs-minor-mode) ;;代码折叠
(require 'init-fci) ;;这个应该是80列缩进线,代码格式化
(require 'init-auto-insert) 

(require 'init-recentf)
(require 'init-helm)
(require 'init-dired)   ;;自带文件管理
(require 'init-hippie-expand)  ;;emacs自带补全 M-/
(require 'init-windows)  ;;窗口管理C-x 2上下,C-x 3左右
(require 'init-modeline)   ;;状态栏


(require 'init-flycheck)
;; (require 'init-spelling)
(require 'init-yasnippet)
(require 'init-company)

;; ;; (require 'init-gnus)

(require 'init-evil)
(require 'init-quickrun)

(require 'init-git)

(require 'init-keybind)

(require 'init-markdown)
(require 'init-rst)
(require 'init-matlab)
(require 'init-sh) ;;shell
(require 'init-web-mode)
(require 'init-javascript)
(require 'init-css)
(require 'init-python)
;; (require 'init-c)

(require 'init-org)

;; (require 'init-slime)
;; (require 'init-common-lisp) ;; lisp
(require 'init-misc) ;;yes or no

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales) ;;一些编码设置


(provide 'init)

(custom-set-variables
 )
(custom-set-faces
 )
 ;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
