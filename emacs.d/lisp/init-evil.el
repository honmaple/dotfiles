;;leader 要在evil-mode前,否则messages无法激活
(use-package evil-leader
  :hook (after-init . global-evil-leader-mode)
  :init
  (defun maple/set-leader-for-startup-buffers ()
    "Set the leader mode for buffers created when Emacs starts."
    (dolist (buffer '("*Messages*" "*Compile-Log*"))
      (when (and (get-buffer buffer)
                 (with-current-buffer buffer
                   (evil-leader-mode 1))))))
  (add-hook 'after-init-hook 'maple/set-leader-for-startup-buffers)
  :config (evil-leader/set-leader ","))


(use-package evil
  :hook (after-init . evil-mode)
  :config
  (progn
    (fset 'evil-visual-update-x-selection 'ignore) ;;粘贴板
    (evil-set-initial-state 'image-mode 'emacs)
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
    ;; (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
    )
  :custom
  (evil-insert-state-cursor '((bar . 2) "chartreuse3"))
  (evil-normal-state-cursor '(box "DarkGoldenrod2"))
  (evil-visual-state-cursor '((hbox . 2) "gray"))
  (evil-emacs-state-cursor '(box "SkyBlue2"))
  (evil-replace-state-cursor '((hbox . 2) "chocolate"))
  :custom-face
  (region ((t (:background "#66d9ef" :foreground "#272822"))))
  :bind (:map evil-normal-state-map
              ("C-k" . evil-scroll-up)
              ("C-j" . evil-scroll-down)))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode)
  :evil-bind
  (visual evil-surround-mode-map
          "s" 'evil-surround-region
          "S" 'evil-substitute))

(use-package evil-matchit
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-ediff
  :after ediff)

(use-package evil-escape
  :hook (after-init . evil-escape-mode)
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
  :diminish evil-mc-mode "ⓒ"
  :hook (after-init . global-evil-mc-mode)
  :config
  (progn
    (evil-define-command evil-mc/undo-cursor-and-quit ()
      "Initialize `evil-mc-pattern', make a cursor at point, and go to the next match."
      :repeat ignore
      :evil-mc t
      (progn
        (evil-mc-undo-all-cursors)
        (hydra-keyboard-quit)))

    (defhydra maple/evil-mc ()
      ("n" evil-mc-make-and-goto-next-match "next")
      ("t" evil-mc-skip-and-goto-next-match "skip and next")
      ("T" evil-mc-skip-and-goto-prev-match "skip and prev")
      ("p" evil-mc-make-and-goto-prev-match "prev")
      ("N" evil-mc-make-and-goto-prev-match "prev")
      ("q" evil-mc/undo-cursor-and-quit "quit")
      )
    ;; (setq evil-mc-enable-bar-cursor nil)
    (define-key evil-visual-state-map (kbd "n") 'maple/evil-mc/body))
  :custom-face
  (evil-mc-cursor-default-face ((t (:inherit cursor :background "firebrick1" :inverse-video nil))))
  (hydra-face-red ((t (:foreground "chocolate" :weight bold))))
  :bind (:map evil-mc-key-map
              ("C-g" . evil-mc-undo-all-cursors)
              :map evil-visual-state-map
              ("C-n" . evil-mc-make-and-goto-next-match)
              ("C-p" . evil-mc-make-and-goto-prev-match)
              ("C-t" . evil-mc-skip-and-goto-next-match))
  :evil-bind
  (normal evil-mc-key-map
          (kbd "<escape>") 'evil-mc-undo-all-cursors))



(use-package expand-region
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
