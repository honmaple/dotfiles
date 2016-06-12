;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)


;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)

(require-package 'linum-relative)
(require 'linum-relative)
(global-linum-mode t)
(setq linum-relative-current-symbol "")
(linum-relative-on)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

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
                   ";; %s\n\n"
                   (replace-regexp-in-string
                    "\n" "\n;; " ; comment each line
                    (replace-regexp-in-string
                     "\n$" ""    ; remove trailing linebreak
                     (shell-command-to-string "fortune"))))
                (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

(provide 'init-gui)
