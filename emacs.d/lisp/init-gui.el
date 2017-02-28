;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
;; 关闭文件滑动控件
(when (featurep 'scroll-bar) (scroll-bar-mode -1))
;; 关闭工具栏
(when (featurep 'tool-bar) (tool-bar-mode -1))
;;关闭菜单栏
(when (featurep 'menu-bar) (menu-bar-mode -1))

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq large-file-warning-threshold 100000000)
(global-hl-line-mode t) ;;高亮当前行


(setq x-select-enable-clipboard t) ;;激活粘贴板
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq-default
 blink-cursor-interval 0.4
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
 ad-redefinition-action 'accept
 )


(setq indicate-empty-lines t
      transient-mark-mode nil)

(setq backup-directory-alist `(("." . ,(concat maple-cache-directory "auto-save"))))

(use-package bookmark
  :defer t
  :init
  (progn
    (setq bookmark-default-file (concat maple-cache-directory "bookmarks")
          ;; autosave each change
          bookmark-save-flag 1))
  :bind (:map evil-leader--default-map
              ("fb" . bookmark-jump)))

;;美化显示符号
(use-package global-prettify-symbols-mode
  :defer t
  :init (global-prettify-symbols-mode)
  )
;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------

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
                     (shell-command-to-string
                      "fortune -a | fmt -80 -s | cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n")))
                   (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")
                   )))

(defun maple/show-init-time ()
  (message "Emacs load finished in %.2fms"
           (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))))

(add-hook 'after-init-hook 'maple/show-init-time)


(provide 'init-gui)
