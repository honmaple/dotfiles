;; 注释
(defun maple/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (save-excursion
    (when (hs-already-hidden-p)
      (end-of-visual-line)
      (evil-visual-state)
      (beginning-of-visual-line))
    (let (beg end)
      (if (region-active-p)
          (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
      (comment-or-uncomment-region beg end))))

(defun maple/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun maple/reload-user-init-file()
  (interactive)
  (load-file user-init-file))


(use-package adaptive-wrap
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)))

;; 修改外部文件自动载入
(use-package autorevert
  :defer t
  ;; :init (global-auto-revert-mode)
  :init (add-hook 'after-init-hook #'global-auto-revert-mode)
  :diminish auto-revert-mode
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t
          auto-revert-verbose nil)
    ))

(use-package hide-comnt
  :ensure t
  :commands hide/show-comments-toggle)

(use-package semantic
  :ensure t
  :defer t
  :init
  (progn
    (setq srecode-map-save-file
          (concat maple-cache-directory "srecode-map.el"))
    (setq semanticdb-default-save-directory
          (concat maple-cache-directory "semanticdb/"))
    ;; (add-hook 'after-init-hook 'semantic-mode)
    )
  :config
  (progn
    (add-to-list 'semantic-default-submodes
                 'global-semantic-stickyfunc-mode)
    (add-to-list 'semantic-default-submodes
                 'global-semantic-idle-summary-mode)))

(use-package stickyfunc-enhance
  :ensure t
  :defer t
  :config
  (defun maple/lazy-load-stickyfunc-enhance ()
    "Lazy load the package."
    (require 'stickyfunc-enhance)))


(use-package electric
  :defer t
  :init (electric-pair-mode 1)
  :config
  (progn
    ;; (setq electric-pair-pairs '((?\' . ?\')))
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)))

(use-package page
  :init
  (progn
    ;; Don't disable narrowing commands
    (put 'narrow-to-region 'disabled nil)
    (put 'narrow-to-page 'disabled nil)
    (put 'narrow-to-defun 'disabled nil)
    ))

;; (use-package which-func
;;   :init (add-hook 'after-init-hook 'which-function-mode))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :config
  (progn
    ;; enable eldoc in `eval-expression'
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    ;; enable eldoc in IELM
    (add-hook 'ielm-mode-hook #'eldoc-mode)
    ;; don't display eldoc on modeline
    ))


(provide 'init-editor)
