;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun maple/open-line-with-reindent (n) ;; 跳转到原来的位置
  "A version of `open-line' which reindents the start and end positions.
  If there is a fill prefix and/or a `left-margin', insert them
  on the new line if the line would have been blank.
  With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'maple/open-line-with-reindent)

;; 注释
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun maple/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun maple/reload-user-init-file()
  (interactive)
  (load-file user-init-file))

(global-set-key [f6] 'maple/indent-buffer)

(use-package adaptive-wrap
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
    (setq srecode-map-save-file (concat maple-cache-directory
                                        "srecode-map.el"))
    (setq semanticdb-default-save-directory (concat maple-cache-directory
                                                    "semanticdb/"))
    (unless (file-exists-p semanticdb-default-save-directory)
      (make-directory semanticdb-default-save-directory)))
  :config
  (progn
    (add-to-list 'semantic-default-submodes
                 'global-semantic-stickyfunc-mode)
    (add-to-list 'semantic-default-submodes
                 'global-semantic-idle-summary-mode)
    (semantic-mode 1)
    ))

(use-package stickyfunc-enhance
  :ensure t
  :defer t
  :config
  (defun maple/lazy-load-stickyfunc-enhance ()
    "Lazy load the package."
    (require 'stickyfunc-enhance)))

(use-package electric
  :init (electric-pair-mode)
  :config
  (progn
    (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
    ))

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

(use-package dumb-jump
  :ensure t
  :defer t
  :config (setq dumb-jump-selector 'helm)
  :bind (:map evil-normal-state-map
              ("gd" . dumb-jump-go-other-window)))


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
