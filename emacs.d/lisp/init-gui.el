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
  ;; (setq initial-major-mode 'fundamental-mode)
  (fset 'display-startup-echo-area-message 'ignore)
  (setq inhibit-startup-screen t
        initial-scratch-message (maple/initial-message ";; ")))

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
              major-mode 'conf-mode
              fill-column 80
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
              x-gtk-use-system-tooltips nil
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
  :config
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable"))

;;高亮当前行
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (concat maple-cache-directory "bookmarks")
        bookmark-save-flag 1)
  :evil-leader ("fb" . bookmark-jump))

(provide 'init-gui)
