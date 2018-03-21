;;; Find and load the correct package.el

;; enter表示安装,d表示删除,x表示执行删除

(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org"   . "http://elpa.emacs-china.org/org/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
                         ))
(setq package-enable-at-startup nil)
(package-initialize)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose nil
      use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-minimum-reported-time 0.01)

(use-package evil-use-package
  :demand t
  :load-path "site-lisp/use-package")

;;显示状态mode
(use-package diminish)

;;缓冲区
(use-package scratch)

(use-package async-bytecomp
  :ensure async
  :hook (after-init . async-bytecomp-package-mode)
  :config
  (setq async-bytecomp-allowed-packages '(all)))

;; (use-package benchmark-init
;;   :demand t
;;   :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package package-utils
  :commands (package-utils-with-packages-list)
  :init
  (defun package-upgrade()
    "Upgrade packages."
    (interactive)
    (package-refresh-contents)
    (let ((packages (package-utils-with-packages-list t
                                                      (mapcar #'cdr (package-menu--find-upgrades)))))
      (if packages
          (when (yes-or-no-p
                 (message "Upgrade %d package%s (%s)? "
                          (length packages)
                          (if (= (length packages) 1) "" "s")
                          (mapconcat #'package-desc-full-name packages ",")))
            (package-utils-with-packages-list t
                                              (package-menu-mark-upgrades)
                                              (package-menu-execute t)))
        (message "All packages are already up to date.")))))

(use-package fullframe
  :config (fullframe list-packages quit-window))

(use-package cl-lib)

(use-package restart-emacs)
;; :config (setq restart-emacs-restore-frames t))

(use-package exec-path-from-shell
  :if maple-system-is-mac
  :init (exec-path-from-shell-initialize))

(use-package server
  :ensure nil
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-mode)))

(provide 'init-elpa)

;;; init-elpa.el ends here
