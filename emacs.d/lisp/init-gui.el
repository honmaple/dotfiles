;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
;; 关闭文件滑动控件
(when (featurep 'scroll-bar) (scroll-bar-mode -1))
;; 关闭工具栏
(when (featurep 'tool-bar) (tool-bar-mode -1))
;;关闭菜单栏
(when (featurep 'menu-bar) (menu-bar-mode -1))

;; 和上面一样，但要快一些
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(scroll-bar-lines . 0) default-frame-alist)
(defun maple/startup()
  "Set startup."
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        initial-scratch-message
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
             ))))

(add-hook 'after-init-hook #'maple/startup)

(setq use-file-dialog nil
      use-dialog-box nil
      window-combination-resize t
      indicate-empty-lines t
      transient-mark-mode nil
      backup-directory-alist `(("." . ,(concat maple-cache-directory "auto-save")))
      select-enable-clipboard t) ;;激活粘贴板

;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; important for golden-ratio to better work
(setq-default tab-width 4
              regex-tool-backend 'perl
              blink-cursor-interval 0.4
              buffers-menu-max-size 30
              case-fold-search t
              column-number-mode t
              ;; delete-selection-mode t ;;粘贴删除选中区域
              indent-tabs-mode nil
              make-backup-files nil ;;禁止生成类似init.el~文件
              save-interprogram-paste-before-kill t
              set-mark-command-repeat-pop t
              tooltip-delay 1
              truncate-lines t
              truncate-partial-width-windows t
              ad-redefinition-action 'accept)

(setq frame-title-format
      '("GNU Emacs " emacs-version " "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq mouse-yank-at-point t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      xterm-mouse-mode 1)
;; 光标位于中间
(setq scroll-preserve-screen-position t
      scroll-margin 15
      scroll-conservatively 101)

;; 设置默认浏览器
(use-package browse-url
  :ensure nil
  :init
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable"))

;;高亮当前行
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package ediff
  :ensure nil
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package bookmark
  :ensure nil
  :init
  (setq bookmark-default-file (concat maple-cache-directory "bookmarks")
        bookmark-save-flag 1)
  :evil-leader ("fb" . bookmark-jump))


(provide 'init-gui)
