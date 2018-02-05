;;; Find and load the correct package.el

;; enter表示安装,d表示删除,x表示执行删除
(require 'package)

;;; Standard package repositories

;; We include the org repository for completeness, but don't normally
;; use it.

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org"   . "http://orgmode.org/elpa/")
                         ))

(setq package-enable-at-startup nil)

(package-initialize)

(defun package-upgrade ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; (setq use-package-verbose t)
;; (setq use-package-always-ensure t)
;; (setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(use-package evil-use-package
  :load-path "site-lisp/use-package")

;;显示状态mode
(use-package diminish
  :ensure t
  :defer t)

;;缓冲区
(use-package scratch
  :ensure t
  :defer t)

;; 命令行历史
(use-package mwe-log-commands
  :ensure t
  :defer t)

(use-package async-bytecomp
  :ensure async
  :defer t
  :init
  (add-hook 'after-init-hook #'async-bytecomp-package-mode)
  :config
  (setq async-bytecomp-allowed-packages '(all)))

;; (use-package benchmark-init
;;   :ensure t
;;   :init
;;   (add-hook 'after-init-hook #'benchmark-init/deactivate))

(use-package package-utils
  :ensure t
  :defer t)

(use-package fullframe
  :ensure t
  :defer t
  :config (fullframe list-packages quit-window))

(use-package cl-lib
  :ensure t)

(use-package restart-emacs
  :ensure t
  :defer t)
;; :config (setq restart-emacs-restore-frames t))

(use-package exec-path-from-shell
  :if maple-system-is-mac
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package server
  :defer t
  :config
  (unless (server-running-p)
    (server-start)))


(provide 'init-elpa)

;;; init-elpa.el ends here
