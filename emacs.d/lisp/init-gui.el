;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(tool-bar-mode -1)  ;; 关闭工具栏
(menu-bar-mode -1)  ;;关闭菜单栏
(scroll-bar-mode -1) ;; 关闭文件滑动控件
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(global-hl-line-mode t) ;;高亮当前行


(setq x-select-enable-clipboard t) ;;激活粘贴板
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t ;;粘贴删除选中区域
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil ;;禁止生成类似init.el~文件
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines t
 truncate-partial-width-windows t
 ad-redefinition-action 'accept)

(global-auto-revert-mode) ;; 修改外部文件自动载入
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode)) ;;美化显示符号
;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)

(require-package 'linum-relative)
(use-package linum-relative ;;相对行号
  :commands (linum-relative-toggle linum-relative-on)
  :init
  (progn
    (global-linum-mode t)
    (linum-relative-on))
  :config (setq linum-relative-current-symbol ""))
;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; 启动显示信息
(setq-default initial-scratch-message
              (if (executable-find "fortune")
                  (format
                   ";; %s\n\n%s"
                   (replace-regexp-in-string
                    "\n" "\n;; " ; comment each line
                    (replace-regexp-in-string
                     "\n$" ""    ; remove trailing linebreak
                     (shell-command-to-string "fortune")))
                   (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))))

(provide 'init-gui)
