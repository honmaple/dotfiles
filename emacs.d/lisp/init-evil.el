;;leader 要在evil-mode前,否则messages无法激活
(use-package evil-leader
  :ensure t
  :defer t
  :init (global-evil-leader-mode)
  ;; :init (add-hook 'after-init-hook #'global-evil-leader-mode)
  :config (evil-leader/set-leader ","))


(use-package evil
  :ensure t
  :defer t
  ;; :init (evil-mode 1)
  :init (add-hook 'after-init-hook #'evil-mode)
  :config
  (progn
    (fset 'evil-visual-update-x-selection 'ignore) ;;粘贴板
    ;; (evil-set-initial-state 'sql-mode 'emacs)
    (evil-set-initial-state 'image-mode 'emacs)
    (evil-set-initial-state 'inferior-python-mode 'emacs)

    (add-hook 'view-mode-hook #'evil-emacs-state)

    (setq evil-insert-state-cursor '((bar . 2) "chartreuse3")
          evil-normal-state-cursor '(box "DarkGoldenrod2")
          evil-visual-state-cursor '((hbox . 2) "gray")
          evil-emacs-state-cursor '(box "SkyBlue2")
          evil-replace-state-cursor '((hbox . 2) "chocolate"))
    (custom-set-faces
     '(region ((t (:background "#66d9ef" :foreground "#272822")))))

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
  :ensure t
  :defer t
  ;; (global-evil-surround-mode 1)
  :init (add-hook 'after-init-hook #'global-evil-surround-mode)
  :config
  (progn
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
    ))

(use-package evil-matchit
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook #'global-evil-matchit-mode))
;; :init (global-evil-matchit-mode 1))

(use-package evil-ediff
  :ensure t
  :after (ediff)
  )

(use-package evil-escape
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook #'evil-escape-mode)
  ;; :init (evil-escape-mode 1)
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
                                             term-mode
                                             org-agenda-mode
                                             undo-tree-visualizer-mode
                                             newsticker-treeview-mode
                                             newsticker-treeview-list-mode
                                             newsticker-treeview-item-mode
                                             imenu-list-major-mode))
    (setq evil-escape-inhibit-functions '(evil-visual-state-p evil-escape--is-magit-buffer))
    ))

(use-package evil-mc
  :ensure t
  :defer t
  :diminish evil-mc-mode "ⓒ"
  ;; :init (global-evil-mc-mode t)
  :init (add-hook 'after-init-hook #'global-evil-mc-mode)
  :config
  (progn
    (defhydra maple/evil-mc ()
      ("n" evil-mc-make-and-goto-next-match "next")
      ("t" evil-mc-skip-and-goto-next-match "skip")
      ("p" evil-mc-make-and-goto-prev-match "prev")
      ("N" evil-mc-make-and-goto-prev-match "prev"))
    (define-key evil-visual-state-map (kbd "n") 'maple/evil-mc/body)
    ;; (setq evil-mc-enable-bar-cursor nil)
    (custom-set-faces
     '(evil-mc-cursor-default-face ((t (:inherit cursor :background "firebrick1" :inverse-video nil))))
     '(hydra-face-red ((t (:foreground "chocolate" :weight bold)))))
    (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors))
  :bind (:map evil-mc-key-map
              ("C-g" . evil-mc-undo-all-cursors)
              ))


(use-package vimish-fold
  :ensure t
  :defer t
  :bind (:map evil-visual-state-map
              ("za" . vimish-fold)
              :map evil-normal-state-map
              ("zc" . vimish-fold-delete))
  )


(use-package expand-region
  :ensure t
  :defer t
  :after evil
  :bind (:map evil-visual-state-map
              ("v" . er/expand-region)
              ("V" . er/contract-region)
              ("ew" . er/mark-word)
              ("es" . er/mark-symbol)
              ("eu" . er/mark-url)
              ("ee" . er/mark-email)
              ("ed" . er/mark-defun)
              ("ec" . er/mark-comment)
              ("ep" . er/mark-inside-pairs)))


(provide 'init-evil)
