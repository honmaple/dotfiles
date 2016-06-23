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
(require 'init-rss)

(require 'init-keybind)

(require 'init-markdown)
(require 'init-rst)
(require 'init-matlab)
(require 'init-sh) ;;shell
(require 'init-web-mode)
(require 'init-javascript)
(require 'init-css)
(require 'init-python)
(require 'init-c)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "6df30cfb75df80e5808ac1557d5cc728746c8dbc9bc726de35b15180fa6e0ad9" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default)))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
