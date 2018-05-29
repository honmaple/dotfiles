;;leader 要在evil-mode前,否则messages无法激活
(use-package evil-leader
  :hook (after-init . global-evil-leader-mode)
  :config (evil-leader/set-leader ","))

(use-package evil
  :hook (after-init . evil-mode)
  :config
  (fset 'evil-visual-update-x-selection 'ignore) ;;粘贴板
  ;; (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
  (setq evil-insert-state-cursor '((bar . 2) "chartreuse3")
        evil-normal-state-cursor '(box "DarkGoldenrod2")
        evil-visual-state-cursor '((hbox . 2) "gray")
        evil-emacs-state-cursor '(box "SkyBlue2")
        evil-replace-state-cursor '((hbox . 2) "chocolate"))
  :custom-face
  (region ((t (:background "#66d9ef" :foreground "#272822"))))
  :bind (:map evil-normal-state-map
              ("C-k" . evil-scroll-up)
              ("C-j" . evil-scroll-down)))

(use-package evil-numbers
  :after evil
  :bind
  (:map evil-normal-state-map
        ("+" . evil-numbers/inc-at-pt)
        ("-" . evil-numbers/dec-at-pt)))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode)
  :evil-bind
  (visual evil-surround-mode-map
          "s" 'evil-surround-region
          "S" 'evil-substitute))

(use-package evil-matchit
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-ediff
  :commands (evil-ediff-mode)
  :hook (ediff-mode . evil-ediff-mode))

(use-package evil-escape
  :hook (after-init . evil-escape-mode)
  :diminish 'evil-escape-mode
  :config
  (setq evil-escape-key-sequence "jj"
        evil-escape-delay 0.4
        evil-escape-excluded-major-modes '(dired-mode
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
                                           imenu-list-major-mode)
        evil-escape-inhibit-functions '(evil-visual-state-p
                                        evil-escape--is-magit-buffer)))

(use-package evil-multiedit
  :after evil
  :load-path "site-lisp/evil-multiedit"
  :config
  (setq evil-multiedit-follow-matches t
        evil-multiedit-state-cursor '(box "firebrick1"))
  (defhydra maple/evil-multiedit ()
    ("n" evil-multiedit-match-and-next "next")
    ("t" evil-multiedit-toggle-or-restrict-region "skip and next")
    ("p" evil-multiedit-match-and-prev "prev"))
  :custom-face
  (iedit-occurrence ((t (:background "chocolate" :foreground "#272822"))))
  (hydra-face-red ((t (:foreground "chocolate" :weight bold))))
  :bind (:map evil-visual-state-map
              ("n" . maple/evil-multiedit/body)
              ("C-n" . evil-multiedit-match-and-next)
              ("C-p" . evil-multiedit-match-and-prev)
              ("C-t" . evil-multiedit-make-skip-and-next)
              :map evil-multiedit-state-map
              ("C-n" . evil-multiedit-match-and-next)
              ("C-p" . evil-multiedit-match-and-prev)
              ("C-t" . evil-multiedit-make-skip-and-next)))


(use-package expand-region
  :after evil
  :bind (:map evil-visual-state-map
              ("v" . er/expand-region)
              ("V" . er/contract-region)))


(provide 'init-evil)
