(require-package 'evil)
(require-package 'evil-surround)
(require-package 'evil-matchit)
(require-package 'evil-escape)
(require-package 'expand-region)
(require-package 'evil-mc)
(require-package 'evil-leader)
(require-package 'vimish-fold)


;;leader 要在evil-mode前,否则messages无法激活
(use-package evil-leader
  :defer t
  :init (global-evil-leader-mode)
  :config (evil-leader/set-leader ","))


(use-package evil
  :defer t
  :init (evil-mode 1)
  :config
  (progn
    (fset 'evil-visual-update-x-selection 'ignore) ;;粘贴板
    ;; (evil-set-initial-state 'sql-mode 'emacs)
    (evil-set-initial-state 'image-mode 'emacs)
    (evil-set-initial-state 'inferior-python-mode 'emacs)
    (add-hook 'view-mode-hook #'evil-emacs-state)

    ;; (defun evil-paste-after-from-0 ()
    ;;   (interactive)
    ;;   (let ((evil-this-register ?0))
    ;;     (call-interactively 'evil-paste-after)))

    ;; (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)
    ;; (defun my-save-if-bufferfilename ()
    ;; (if (buffer-file-name)
    ;;     (progn
    ;;       (save-buffer)
    ;;       )
    ;;   (message "no file is associated to this buffer: do nothing")
    ;;   )
    ;; )
    ;; (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
    ;; (add-hook 'evil-insert-state-exit-hook 'my-save-if-bufferfilename)
    )
  :bind (:map evil-normal-state-map
              ("C-k" . evil-scroll-up)
              ("C-j" . evil-scroll-down)))

(use-package evil-surround
  :defer t
  :init (global-evil-surround-mode 1))

(use-package evil-matchit
  :defer t
  :init (global-evil-matchit-mode 1))


(use-package evil-escape
  :defer t
  :init (evil-escape-mode 1)
  :diminish 'evil-escape-mode
  :config
  (progn
    (setq-default evil-escape-key-sequence "jj")
    (setq-default evil-escape-delay 0.4)
    (setq evil-escape-excluded-major-modes '(dired-mode
                                             neotree-mode
                                             help-mode
                                             magit-mode
                                             shell-mode
                                             org-agenda-mode
                                             undo-tree-visualizer-mode
                                             newsticker-treeview-mode
                                             newsticker-treeview-list-mode
                                             newsticker-treeview-item-mode))
    (setq evil-escape-inhibit-functions '(evil-visual-state-p evil-escape--is-magit-buffer))
    ))

(use-package evil-mc
  :defer t
  :diminish evil-mc-mode "ⓒ"
  :init (global-evil-mc-mode 1)
  :config
  (progn
    (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors)
    )
  :bind (:map evil-mc-key-map
              ("C-g" . evil-mc-undo-all-cursors)
              ))


(use-package vimish-fold
  :defer t
  :bind (:map evil-visual-state-map
              ("za" . vimish-fold)
              :map evil-normal-state-map
              ("zc" . vimish-fold-delete))
  )


(use-package expand-region
  :defer t
  :bind (:map evil-visual-state-map
              ("v" . er/expand-region)
              ("ew" . er/mark-word)
              ("es" . er/mark-symbol)
              ("eu" . er/mark-url)
              ("ee" . er/mark-email)
              ("ed" . er/mark-defun)
              ("ec" . er/mark-comment)
              ("ep" . er/mark-inside-pairs)))


(provide 'init-evil)
